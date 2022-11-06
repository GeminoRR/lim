Imports System.IO

'==============================
'========== LIM FILE ==========
'==============================
Public Class LimFile

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public compiler As VB_Compiler
    Public name As String
    Public path As String
    Public content As String = ""

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal path As String, ByRef compiler As VB_Compiler)

        'Set path
        Me.path = path.Replace("\", "/")

        'Set name
        If Me.path.Contains("/") Then
            Me.name = Me.path.Substring(Me.path.LastIndexOf("/"))
        Else
            Me.name = path
        End If

        'Set compiler
        Me.compiler = compiler

        'No file
        If Not File.Exists(Me.path) Then
            addBasicError("file not found", "The file """ & Me.path & """ does not exist.")
        End If
        Try
            content = File.ReadAllText(Me.path)
        Catch ex As Exception
            addBasicError("Unable to read file", ex.Message)
        End Try

        'Compile tokens (Keywords)
        Dim tokens As List(Of token) = (New lexer()).parse(Me)

        'Generate AST (Abstract syntax tree)


    End Sub

End Class