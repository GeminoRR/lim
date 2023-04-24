'=============================================
'========== FUNCTION CALL STATEMENT ==========
'=============================================
'
' Represents a direct addition of source code
'
Class AddSourceDirectlyStatementNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private AsdNode As AddSourceDirectlyNode

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal AsdNode As AddSourceDirectlyNode)

        'Inherits
        MyBase.New(AsdNode.PositionStartY, AsdNode.PositionStartX, AsdNode.PositionEndY, AsdNode.PositionEndX)

        'Properties
        Me.AsdNode = AsdNode
        Me.AsdNode.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return Me.AsdNode.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))
        Content.Add(Me.AsdNode.Compile(Content))
    End Sub

End Class
