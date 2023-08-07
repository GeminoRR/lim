'==============================
'========== NEW NODE ==========
'==============================
'
' Represents an object instantiation.
'
Class NewNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public TargetType As TypeNode
    Public PassedArguments As List(Of ValueNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As NewNode = Me.MemberwiseClone()
        Cloned.TargetType = Cloned.TargetType.Clone(Cloned)
        Cloned.PassedArguments = New List(Of ValueNode)
        For Each i As ValueNode In Me.PassedArguments
            Cloned.PassedArguments.Add(i.Clone(Cloned))
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal TargetType As TypeNode, ByVal PassedArguments As List(Of ValueNode))

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.TargetType = TargetType
        Me.TargetType.ParentNode = Me
        Me.PassedArguments = PassedArguments
        For Each Arg As ValueNode In Me.PassedArguments
            Arg.ParentNode = Me
        Next

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Arguments_STR As String = ""
        For Each arg As ValueNode In Me.PassedArguments
            Arguments_STR &= ", (" & arg.ToString() & ")"
        Next
        If Arguments_STR.StartsWith(", ") Then
            Arguments_STR = "(" & Arguments_STR.Substring(2) & ")"
        End If
        Return "new " & TargetType.ToString() & Arguments_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Check if type has a constructor
        For Each Method As FunctionNode In Me.TargetType.AssociateType.Methods
            If Method.FunctionName = "new" Then

                'Arguments count
                If Me.PassedArguments.Count < Method.MinArguments Then
                    Dim missing As Integer = Method.MinArguments - Me.PassedArguments.Count
                    If missing = 1 Then
                        ThrowNodeSyntaxException("NNC01", "1 argument is missing.", Me)
                    Else
                        ThrowNodeSyntaxException("NNC01", missing.ToString() & " arguments are missing.", Me)
                    End If
                End If
                If Me.PassedArguments.Count > Method.MaxArguments Then
                    Dim missing As Integer = Me.PassedArguments.Count - Method.MaxArguments
                    If missing = 1 Then
                        ThrowNodeSyntaxException("NNC02", "1 argument in excess.", Me)
                    Else
                        ThrowNodeSyntaxException("NNC02", missing.ToString() & " arguments in excess.", Me)
                    End If
                End If

                'Arguments
                Dim Arguments As String = "GV"
                For i As Integer = 0 To Method.FunctionArguments.Count - 1

                    If i < Me.PassedArguments.Count Then

                        'Arguments & Model
                        Dim CurrentArgumentModel As FunctionArgumentNode = Method.FunctionArguments(i)
                        Dim CurrentArgument As ValueNode = Me.PassedArguments(i)

                        'Error with type
                        If Not CurrentArgumentModel.ArgumentType = CurrentArgument.ReturnType Then
                            ThrowNodeSyntaxException("NNC03", " The argument is of type """ & CurrentArgument.ReturnType.ToString() & """ while the function was expecting an argument of type """ & CurrentArgumentModel.ArgumentType.ToString() & """.", CurrentArgument)
                        End If

                        'Compile
                        Arguments &= ", (" & CurrentArgument.Compile(content) & ")"

                    Else

                        'Optional
                        Arguments &= ", NULL"

                    End If

                Next

                'Compile
                Method.Compile(Nothing)

                'Return
                Return Method.CompiledName & "(" & Arguments & ")"

            End If
        Next

        'No constructor, use allocate()
        Return Me.TargetType.AssociateType.CompiledName & "_allocate(GV)"

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
        Return Me.TargetType.AssociateType
    End Function

End Class
