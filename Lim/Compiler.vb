'==============================
'========== COMPILER ==========
'==============================
'
' Main file.
'

Module Compiler

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