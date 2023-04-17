Imports System.IO
'==============================
'========== COMPILER ==========
'==============================
'
' Main file.
'

Module Compiler

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public executableDirectory As String = Directory.GetParent(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName).FullName.Replace("\", "/")
    Public ReadOnly AppData As String = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData).Replace("\", "/") & "/Lim"

    '==================================
    '========== GET TEMPLATE ==========
    '==================================
    Public Function ReadTemplateFile(ByVal filepath As String) As String
        If Not File.Exists(filepath) Then
            ThrowSimpleLimException("CGT01", "Cannot read file", """" & filepath & """ is missing")
        End If
        Dim result As String = ""
        Try
            result = File.ReadAllText(filepath)
        Catch ex As Exception
            ThrowSimpleLimException("CGT02", "Cannot read file", ex.Message)
        End Try
        Return result
    End Function

    'Variables
    Public AllImportedFiles As New List(Of SourceFile)
    Public MainFile As SourceFile


    'Compile
    Public Sub compile(ByVal inputFile As String)

        'Add main file
        MainFile = New SourceFile(inputFile)
        AllImportedFiles.Add(MainFile)

    End Sub

End Module