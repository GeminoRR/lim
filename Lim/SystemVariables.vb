'============================
'========= VARIABLE =========
'============================
Public Class Variable

    Public type As Type
    Public name As String
    Public compiledName As String
    Public doubleRef As Boolean

    Public Sub New(ByVal name As String, ByVal type As Type, ByVal compiledName As String, Optional ByVal doubleRef As Boolean = False)

        Me.name = name
        Me.type = type
        Me.compiledName = compiledName
        Me.doubleRef = doubleRef

    End Sub

End Class