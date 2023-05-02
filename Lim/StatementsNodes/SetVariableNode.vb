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
        Dim TempVar As String = GetVariableCompiledName()
        Content.Add(Target.ReturnType.CompiledName & " ** =  (" & Target.Compile(Content) & ");")
        Content.Add("(" & Target.Compile(Content) & ") = (" & NewValue.Compile(Content) & ");")

    End Sub

End Class
