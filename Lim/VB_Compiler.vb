Imports System.IO
Public Class VB_Compiler

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public files As New List(Of LimFile)

    '================================
    '========== MAIN ENTRY ==========
    '================================
    Public Sub New(ByVal inputFile As String, ByVal ouputFolder As String)

        'Analyse first file
        files.Add(New LimFile(inputFile, Me))

    End Sub

End Class
