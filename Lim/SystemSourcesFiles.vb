Imports System.IO

'==========================================
'========== SYSTEM SOURCES FILES ==========
'==========================================
'
' This class represents a code file.
'
Public Class SourceFile

    'Properties
    Public LimLib As Boolean = False
    Public filename As String
    Public filepath As String
    Public lines As New List(Of String)

    'Constructor
    Public Sub New(ByVal filepath As String)

        'Fix filename
        filepath = filepath.Replace("\", "/")
        Me.filepath = filepath
        Me.filename = filepath.Substring(filepath.LastIndexOf("/") + 1)

        'File exist
        If Not File.Exists(filepath) Then
            ThrowSimpleLimException("SSFN01", "Missing file", "File """ & Me.filename & """ could not be found")
        End If

        'Read file
        Try

            Dim reader As New StreamReader(filepath)
            Do Until reader.EndOfStream
                lines.Add(reader.ReadLine())
            Loop

        Catch ex As Exception

            ThrowSimpleLimException("SSFN02", "Unable to read file", "Unable to read the """ & Me.filename & """ file, it may be used by another program?")

        End Try

        'Token
        Dim tokens As List(Of Token) = LexerParse(lines, Me)

        For Each i As Token In tokens
            Console.WriteLine(i.ToString())
        Next

        'Preprocessing


    End Sub

End Class
