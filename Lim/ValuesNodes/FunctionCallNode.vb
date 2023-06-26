'========================================
'========== FUNCTION CALL NODE ==========
'========================================
'
' Represents a call to a function
'
Class FunctionCallNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public TargetFunction As ValueNode
    Public PassedArguments As New List(Of ValueNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As FunctionCallNode = Me.MemberwiseClone()
        Cloned.TargetFunction = Cloned.TargetFunction.Clone(Cloned)
        Cloned.PassedArguments = New List(Of ValueNode)
        For Each i As ValueNode In Me.PassedArguments
            Cloned.PassedArguments.Add(i.Clone(Cloned))
        Next
        Return Cloned

    End Function

    '==============================================
    '========== DIRECT TARGETED FUNCTION ==========
    '==============================================
    Private Sub UpdateDirectTargetedFunction()

        'Already searched
        If AlreadySearch Then
            Exit Sub
        End If
        AlreadySearch = True

        'Variables
        DirectTargetedFunction = Nothing
        DirectTargetedInstance = Nothing

        'Call on a variable
        If TypeOf TargetFunction Is VariableNode Then

            'Get function
            DirectTargetedFunction = DirectCast(TargetFunction, VariableNode).GetFunction()

            'Parent is a class
            If DirectTargetedFunction IsNot Nothing Then
                If TypeOf DirectTargetedFunction.ParentNode Is Type Then
                    DirectTargetedInstance = "self"
                End If
            End If

        ElseIf TypeOf TargetFunction Is ChildNode Then

            'Get function
            DirectTargetedFunction = DirectCast(TargetFunction, ChildNode).TargetedFunction()

            'Get instance
            If DirectTargetedFunction IsNot Nothing Then
                DirectTargetedInstance = DirectCast(TargetFunction, ChildNode).Obj
            End If

        End If

    End Sub
    Private DirectTargetedFunction As FunctionNode
    Private DirectTargetedInstance As Object
    Private AlreadySearch As Boolean = False

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal TargetFunction As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.TargetFunction = TargetFunction
        Me.TargetFunction.ParentNode = Me

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Arguments_STR As String = ""
        For Each arg As Node In PassedArguments
            Arguments_STR &= ", (" & arg.ToString() & ")"
        Next
        If Arguments_STR.StartsWith(", ") Then
            Arguments_STR = Arguments_STR.Substring(2)
        End If
        Arguments_STR = "(" & Arguments_STR & ")"
        Return TargetFunction.ToString() & Arguments_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Update
        UpdateDirectTargetedFunction()

        If DirectTargetedFunction Is Nothing Then

            'Type
            If Not Me.TargetFunction.ReturnType.ParentClass = STDClass_fun Then
                ThrowNodeTypeException("FCNC00", "A function was expected here, but the type is """ & Me.TargetFunction.ReturnType.ToString() & """.", Me.TargetFunction)
            End If

            'Get function
            Dim TempFun As String = GetVariableCompiledName()
            content.Add(Me.TargetFunction.ReturnType.CompiledName & " * " & TempFun & " = " & Me.TargetFunction.Compile(content) & ";")

            'Arguments count
            If Me.PassedArguments.Count < (Me.TargetFunction.ReturnType.PassedArguments.Count - 1) Then
                Dim missing As Integer = (Me.TargetFunction.ReturnType.PassedArguments.Count - 1) - Me.PassedArguments.Count
                If missing = 1 Then
                    ThrowNodeSyntaxException("FCNC01", "1 argument is missing.", Me)
                Else
                    ThrowNodeSyntaxException("FCNC01", missing.ToString() & " arguments are missing.", Me)
                End If
            End If
            If Me.PassedArguments.Count > (Me.TargetFunction.ReturnType.PassedArguments.Count - 1) Then
                Dim missing As Integer = Me.PassedArguments.Count - (Me.TargetFunction.ReturnType.PassedArguments.Count - 1)
                If missing = 1 Then
                    ThrowNodeSyntaxException("FCNC02", "1 argument in excess.", Me)
                Else
                    ThrowNodeSyntaxException("FCNC02", missing.ToString() & " arguments in excess.", Me)
                End If
            End If

            'Get arguments
            Dim arguments As String = ""
            For i As Integer = 1 To Me.TargetFunction.ReturnType.PassedArguments.Count - 1

                Dim CurrentArgumentModelType As Type = TargetFunction.ReturnType.PassedArguments(i)
                Dim CurrentArgument As ValueNode = Me.PassedArguments(i - 1)

                If Not CurrentArgumentModelType = CurrentArgument.ReturnType Then
                    ThrowNodeSyntaxException("FCNC03", " The argument is of type """ & CurrentArgument.ReturnType.ToString() & """ while the function was expecting an argument of type """ & CurrentArgumentModelType.ToString() & """.", CurrentArgument)
                End If

                arguments &= ", (" & CurrentArgument.Compile(content) & ")"

            Next

            'Function call
            Return TempFun & "->target(GV, " & TempFun & "->object" & arguments & ")"

        Else

            'Compile function
            DirectTargetedFunction.Compile(Nothing)

            'Arguments count
            If Me.PassedArguments.Count < DirectTargetedFunction.MinArguments Then
                Dim missing As Integer = DirectTargetedFunction.MinArguments - Me.PassedArguments.Count
                If missing = 1 Then
                    ThrowNodeSyntaxException("FCNC04", "1 argument is missing.", Me)
                Else
                    ThrowNodeSyntaxException("FCNC04", missing.ToString() & " arguments are missing.", Me)
                End If
            End If
            If Me.PassedArguments.Count > DirectTargetedFunction.MaxArguments Then
                Dim missing As Integer = Me.PassedArguments.Count - DirectTargetedFunction.MaxArguments
                If missing = 1 Then
                    ThrowNodeSyntaxException("FCNC05", "1 argument in excess.", Me)
                Else
                    ThrowNodeSyntaxException("FCNC05", missing.ToString() & " arguments in excess.", Me)
                End If
            End If


            'Arguments
            Dim Arguments As String = "GV, "
            If DirectTargetedInstance Is Nothing Then
                Arguments &= "NULL"
            Else
                If TypeOf DirectTargetedInstance Is ValueNode Then
                    Arguments &= DirectTargetedInstance.Compile(content)
                Else
                    Arguments &= DirectTargetedInstance.ToString()
                End If
            End If
            For i As Integer = 0 To DirectTargetedFunction.MaxArguments - 1

                If i < Me.PassedArguments.Count Then

                    'Arguments & Model
                    Dim CurrentArgumentModel As FunctionArgumentNode = DirectTargetedFunction.FunctionArguments(i)
                    Dim CurrentArgument As ValueNode = Me.PassedArguments(i)

                    'Error with type
                    If Not CurrentArgumentModel.ArgumentType = CurrentArgument.ReturnType Then
                        ThrowNodeSyntaxException("FCNC06", " The argument is of type """ & CurrentArgument.ReturnType.ToString() & """ while the function was expecting an argument of type """ & CurrentArgumentModel.ArgumentType.ToString() & """.", CurrentArgument)
                    End If

                    'Compile
                    Arguments &= ", (" & CurrentArgument.Compile(content) & ")"

                Else

                    'No passed
                    Arguments &= ", NULL"

                End If

            Next

            'Function call
            Return DirectTargetedFunction.CompiledName & "(" & Arguments & ")"

        End If

    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return False
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type

        'Update
        UpdateDirectTargetedFunction()

        If DirectTargetedFunction Is Nothing Then

            'Type
            If Not Me.TargetFunction.ReturnType.ParentClass = STDClass_fun Then
                ThrowNodeTypeException("FCNNRT01", "A function was expected here, but the type is """ & Me.TargetFunction.ReturnType.ToString() & """.", Me.TargetFunction)
            End If

            'No value
            If Me.TargetFunction.ReturnType.PassedArguments(0) Is Nothing Then
                ThrowNodeSyntaxException("FCNNRT02", "This function returns no value.", Me)
            End If

            'Return
            Return Me.TargetFunction.ReturnType.PassedArguments(0)

        Else

            'No value
            If DirectTargetedFunction.ReturnType Is Nothing Then
                ThrowNodeSyntaxException("FCNNRT03", "This function returns no value.", Me)
            End If

            'Return return type of function
            Return DirectTargetedFunction.ReturnType

        End If

    End Function

End Class