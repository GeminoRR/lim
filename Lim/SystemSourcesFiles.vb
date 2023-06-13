Imports System.IO

'==========================================
'========== SYSTEM SOURCES FILES ==========
'==========================================
'
' This class represents a code file.
'
Class SourceFile
    Inherits ScopeNode

    'Properties
    Public filename As String
    Public filepath As String
    Public lines As New List(Of String)
    Public ImportedFiles As New List(Of SourceFile)

    Public DeclareVariables As New List(Of DeclareVariableNode)
    Public Functions As New List(Of FunctionNode)
    Public Classes As New List(Of ClassNode)
    Public AddSourceDirectlys As New List(Of AddSourceDirectlyStatementNode)
    Public Extends As New List(Of ExtendNode)

    'Clone
    Protected Overrides Function Duplicate() As Node
        Throw New NotImplementedException()
    End Function

    'Constructor
    Public Sub New(ByVal filepath As String)

        'Inherit
        MyBase.New(0, 0, 0, 0)

        'Fix filename
        Me.filename = filepath.Replace("\", "/").Substring(filepath.LastIndexOf("/") + 1)

        'File exist
        If Not File.Exists(filepath) Then
            ThrowSimpleLimException("SSFN01", "Missing file", "File """ & Me.filename & """ could not be found")
        End If
        filepath = Path.GetFullPath(filepath).Replace("\", "/")
        Me.filepath = filepath
        If ShowDebug Then
            Console.WriteLine("[LOGS] Importing """ & filepath & """")
        End If

        'Read file
        Try

            Dim reader As New StreamReader(filepath)
            Do Until reader.EndOfStream
                lines.Add(reader.ReadLine() & vbLf)
            Loop

        Catch ex As Exception

            ThrowSimpleLimException("SSFN02", "Unable to read file", "Unable to read the """ & Me.filename & """ file, it may be used by another program?")

        End Try

        'Token
        Dim tokens As List(Of Token) = Lexer.LexLines(lines, Me)

        'Empty file
        If Not tokens.Count > 1 Then
            Exit Sub
        End If

        'Import std
        If Not Me.filepath = executableDirectory & "/libs/std.lim" Then
            ImportFile(executableDirectory & "/libs/std.lim")
        End If

        'Import
        While tokens(0).Type = TokenType.CODE_LINEINDENTATION

            'Content
            If Not tokens.Count > 1 Then
                Exit While
            End If

            'Import
            If Not tokens(1).Type = TokenType.KW_IMPORT Then
                Exit While
            End If
            tokens.RemoveAt(0)

            'Import
            If tokens(1).Type = TokenType.CT_STRING Then

                'Import file
                ImportFile(tokens(1).Value)

            ElseIf tokens(1).Type = TokenType.CODE_TERM Then

                'Import lib
                ImportFile(executableDirectory & "/libs/" & tokens(1).Value & ".lim")

            ElseIf tokens(1).Type = TokenType.CODE_DOLLAR Then

                'Include
                If Not tokens.Count > 2 Then
                    ThrowCoordinatesSyntaxLimException("SSFN06", "A string must follow the $ sign.", Me, tokens(1).PositionEndY, tokens(1).PositionEndX, tokens(1).PositionEndY, tokens(1).PositionEndX)
                End If
                If Not tokens(2).Type = TokenType.CT_STRING Then
                    ThrowCoordinatesSyntaxLimException("SSFN05", "A string must follow the $ sign.", Me, tokens(1).PositionEndY, tokens(1).PositionEndX, tokens(2).PositionEndY, tokens(2).PositionEndX)
                End If
                If Not Compiler.Compiled_Imports.Contains("#include " & tokens(2).Value) Then

                    Dim ObjectToImport As String = tokens(2).Value
                    If Not (ObjectToImport.StartsWith("""") And ObjectToImport.EndsWith("""")) And Not (ObjectToImport.StartsWith("<") And ObjectToImport.EndsWith(">")) Then

                        'Include ressources in /src

                        Dim RessourcePath As String = Directory.GetParent(Me.filepath).FullName & "/" & ObjectToImport
                        If Directory.Exists(RessourcePath) Then

                            'Include folder
                            Dim TargetDirectoy As String = AppData & "/src/" & Path.GetFileName(RessourcePath)
                            If Not Directory.Exists(TargetDirectoy) Then

                                Directory.CreateDirectory(TargetDirectoy)
                                Microsoft.VisualBasic.FileIO.FileSystem.CopyDirectory(RessourcePath, TargetDirectoy)

                            End If

                        ElseIf File.Exists(RessourcePath) Then

                            'Include file
                            Dim TargetFile As String = AppData & "/src/" & Path.GetFileName(RessourcePath)
                            If Not File.Exists(TargetFile) Then
                                File.Copy(RessourcePath, TargetFile, True)
                            End If

                        Else
                            ThrowSimpleLimException("SSFN07", "", "Unable to find the file/folder """ & Me.filepath & "/" & ObjectToImport & """.")
                        End If
                    Else

                        'Normal include
                        Compiler.Compiled_Imports.Add("#include " & tokens(2).Value)

                    End If

                End If
                tokens.RemoveAt(0)


            ElseIf tokens(1).Type = TokenType.CODE_LINEINDENTATION Then

                'Something is missing
                ThrowCoordinatesSyntaxLimException("SSFN03", "The name of a file or a library must follow the keyword ""import"".", Me, tokens(0).PositionEndY, tokens(0).PositionEndX, tokens(0).PositionEndY, tokens(0).PositionEndX)

            Else

                'Error
                ThrowCoordinatesSyntaxLimException("SSFN04", "The ""import"" keyword must be followed by a file name (in quotes) or a library name.", Me, tokens(1).PositionStartY, tokens(1).PositionStartX, tokens(1).PositionEndY, tokens(1).PositionEndX)

            End If

            'Advance
            tokens.RemoveAt(0)
            tokens.RemoveAt(0)

        End While

        'Import sublib
        Dim RecursiveList As New List(Of SourceFile)
        For Each i As SourceFile In Me.ImportedFiles
            RecursiveList.Add(i)
        Next
        SubImport(RecursiveList)

        'AST (Abstract Syntax Tree)
        Dim Parser As New AST()
        Parser.ParseFile(tokens, Me)

        'Fix export
        Dim Export As Boolean = False
        For Each declareVarible As DeclareVariableNode In Me.DeclareVariables
            If declareVarible.Export Then
                Export = True
                Exit For
            End If
        Next
        If Not Export Then
            For Each fun As FunctionNode In Me.Functions
                If fun.Export Then
                    Export = True
                    Exit For
                End If
            Next
            If Not Export Then
                For Each C As ClassNode In Me.Classes
                    If C.Export Then
                        Export = True
                        Exit For
                    End If
                Next
            End If
        End If

        If Not Export Then
            For Each declareVarible As DeclareVariableNode In Me.DeclareVariables
                declareVarible.Export = True
            Next
            For Each fun As FunctionNode In Me.Functions
                fun.Export = True
            Next
            For Each C As ClassNode In Me.Classes
                C.Export = True
            Next
        End If

    End Sub

    'Sub import
    Private Sub SubImport(ByVal ListOfImportedFiles As List(Of SourceFile))

        For Each i As SourceFile In ListOfImportedFiles
            If Not Me.ImportedFiles.Contains(i) Then
                Me.ImportedFiles.Add(i)
            End If
            SubImport(i.ImportedFiles)
        Next

    End Sub

    'Import File
    Public Sub ImportFile(ByVal filepath As String)

        'Path empty
        If filepath = "" Then
            ThrowCoordinatesLimException("SSFIF01", "Inconsistent import", "Empty filepath", Me, -1, -1, -1, -1)
        End If

        'Check if file is current
        If Path.GetFullPath(filepath).Replace("\", "/") = Me.filepath Then
            ThrowCoordinatesLimException("SSFIF01", "Inconsistent import", "It is impossible for a file to import itself", Me, -1, -1, -1, -1)
        End If

        'Search if file already imported
        For Each AlreadyImportedFile As SourceFile In AllImportedFiles
            If AlreadyImportedFile.filepath = filepath Then
                Me.ImportedFiles.Add(AlreadyImportedFile)
                Exit Sub
            End If
        Next

        'Create file
        Dim importedFile As New SourceFile(filepath)

        'Add references
        Compiler.AllImportedFiles.Add(importedFile)
        Me.ImportedFiles.Add(importedFile)

    End Sub

    'Content
    Public Overrides Sub Compile(content As List(Of String))
        Throw New NotImplementedException()
    End Sub

End Class
