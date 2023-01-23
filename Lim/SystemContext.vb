'===========================
'========= CONTEXT =========
'===========================
Public Class context

    Public variables As New List(Of Variable)
    Public from As Node
    Public upperContext As context

    Public Sub New(ByVal from As Node, Optional ByVal upperContext As context = Nothing)
        Me.from = from
        Me.upperContext = upperContext
    End Sub

End Class
