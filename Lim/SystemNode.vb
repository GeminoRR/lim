'========================
'========= NODE =========
'========================
Public MustInherit Class Node

    Public positionStart As Integer
    Public positionEnd As Integer
    Public parentNode As Node = Nothing

    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer)
        Me.positionStart = positionStart
        Me.positionEnd = positionEnd
    End Sub

    Public Overrides Function ToString() As String
        Return "()"
    End Function

End Class

'==================================
'========= CONTAINER NODE =========
'==================================
Public MustInherit Class containerNode
    Inherits Node

    Public variables As New List(Of Variable)
    Public codes As New List(Of Node)

    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer)
        MyBase.New(positionStart, positionEnd)
    End Sub

    Public Sub addNodeToCode(ByVal node As Node)
        node.parentNode = Me
        codes.Add(node)
    End Sub

End Class