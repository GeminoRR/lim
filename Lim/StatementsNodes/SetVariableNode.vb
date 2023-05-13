'==================================
'========== SET VARIABLE ==========
'==================================
'
' Represents the assignment of a value to a variable
'   foo = bar
'   foor[bar] = foobar
'   foo.bar = foobar
'
Class SetVariableNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private Target As ValueNode
    Private NewValue As ValueNode

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As SetVariableNode = Me.MemberwiseClone()
        Cloned.Target = Cloned.Target.Clone(Cloned)
        Cloned.NewValue = Cloned.NewValue.Clone(Cloned)
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ComparisonNode As ComparisonNode)

        'Inherits
        MyBase.New(ComparisonNode.PositionStartY, ComparisonNode.PositionStartX, ComparisonNode.PositionEndY, ComparisonNode.PositionEndX)

        'Properties
        Me.Target = ComparisonNode.Left
        Me.Target.ParentNode = Me
        Me.NewValue = ComparisonNode.Right
        Me.NewValue.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return Me.Target.ToString() & " = " & Me.NewValue.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Check type compatibility
        If Not Target.ReturnType = NewValue.ReturnType Then
            ThrowNodeTypeException("SVNC01", "The type of the value (" & NewValue.ReturnType.ToString() & ") does not match that of the variable (" & Target.ReturnType.ToString() & ").", Me.NewValue)
        End If

        'Compile
        Content.Add("")
        If TypeOf Target Is VariableNode Then

            Content.Add(DirectCast(Target, VariableNode).CompileRef(Content) & " = (" & NewValue.Compile(Content) & ");")

        ElseIf TypeOf Target Is ChildNode Then

            Content.Add(DirectCast(Target, ChildNode).CompileRef(Content) & " = (" & NewValue.Compile(Content) & ");")

        Else

            ThrowNodeSyntaxException("SVNC02", "Cannot assign a value to this.", Target)

        End If

    End Sub

End Class
