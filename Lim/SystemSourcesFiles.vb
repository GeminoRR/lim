Imports System.IO

'==========================================
'========== SYSTEM SOURCES FILES ==========
'==========================================
'
' This class represents a code file.
'
Public Class SourceFile
    Inherits ScopeNode

    'Properties
    Public filename As String
    Public filepath As String
    Public lines As New List(Of String)
    Public ImportedFiles As New List(Of SourceFile)

    Public DeclareVariables As New List(Of DeclareVariableNode)
    Public Functions As New List(Of FunctionNode)
    Public Classes As New List(Of ClassNode)
    Public AddSourceDirectlys As New List(Of AddSourceDirectlyNode)

    'Constructor
    Public Sub New(ByVal filepath As String)

        'Inherit
        MyBase.New(Nothing, 0, 0, 0, 0)

        'Fix filename
        Me.filename = filepath.Replace("\", "/").Substring(filepath.LastIndexOf("/") + 1)

        'File exist
        If Not File.Exists(filepath) Then
            ThrowSimpleLimException("SSFN01", "Missing file", "File """ & Me.filename & """ could not be found")
        End If
        filepath = Path.GetFullPath(filepath).Replace("\", "/")
        Me.filepath = filepath

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
        Dim tokens As List(Of Token) = LexerParse(lines, Me)

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
            If tokens(1).Type = TokenType.CONSTANT_STRING Then

                'Import file
                ImportFile(tokens(1).Value)

            ElseIf tokens(1).Type = TokenType.CODE_TERM Then

                'Import lib
                ImportFile(executableDirectory & "/libs/" & tokens(1).Value & ".lim")

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

        'AST (Abstract Syntax Tree)

    End Sub

    'Import File
    Public Sub ImportFile(ByVal filepath As String)

        'Search if file already imported
        For Each AlreadyImportedFile As SourceFile In AllImportedFiles
            If AlreadyImportedFile.filepath = Me.filepath Then
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

    'Compile (for node)
    Public Overrides Function Compile(content As List(Of String)) As String
        Return ""
    End Function

End Class
