'=============================================
'========== FUNCTION CALL STATEMENT ==========
'=============================================
'
' Represents a call to a function
'
Class FunctionCallStatementNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public CallNode As FunctionCallNode

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal CallNode As FunctionCallNode)

        'Inherits
        MyBase.New(CallNode.PositionStartY, CallNode.PositionStartX, CallNode.PositionEndY, CallNode.PositionEndX)

        'Properties
        Me.CallNode = CallNode
        Me.CallNode.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return Me.CallNode.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))
        Content.Add("")
        Content.Add(Me.CallNode.Compile(Content) & ";")
    End Sub

End Class
