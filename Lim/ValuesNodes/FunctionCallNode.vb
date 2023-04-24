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

        'Type
        If Not Me.TargetFunction.ReturnType.ParentClass = STD_funClass Then
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
        Return TempFun & "->target(" & TempFun & "->object" & arguments & ")"

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

        'Type
        If Not Me.TargetFunction.ReturnType.ParentClass = STD_funClass Then
            ThrowNodeTypeException("FCNNRT01", "A function was expected here, but the type is """ & Me.TargetFunction.ReturnType.ToString() & """.", Me.TargetFunction)
        End If

        'No value
        If Me.TargetFunction.ReturnType.PassedArguments(0) Is Nothing Then
            ThrowNodeSyntaxException("FCNNRT02", "This function returns no value.", Me)
        End If

        'Return
        Return Me.TargetFunction.ReturnType.PassedArguments(0)

    End Function

End Class