Imports System.IO

'==============================
'========== LIM FILE ==========
'==============================
Public Class LimFile
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public compiler As VB_Compiler
    Public name As String
    Public path As String
    Public content As String = ""
    Public LimLib As Boolean = False

    Public FilesImports As New List(Of LimFile)

    Public variables As New List(Of Variable)
    Public declareVariables As New List(Of DeclareVariableNode)
    Public functions As New List(Of FunctionNode)
    Public classs As New List(Of ClassNode)
    Public addSourceDirectly As New List(Of AddSourceNode)

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal path As String, ByRef compiler As VB_Compiler)

        'Mybase
        MyBase.New(0, 0)

        'Set path
        Me.path = path.Replace("\", "/")

        'Set name
        If Me.path.Contains("/") Then
            Me.name = Me.path.Substring(Me.path.LastIndexOf("/") + 1)
        Else
            Me.name = path
        End If

        'LimLib
        If path.EndsWith(".lim") Then
            LimLib = False
        ElseIf path.EndsWith(".limlib") Then
            LimLib = True
        Else
            addBasicError("Unsupported file type", "The """ & Me.name & """ file is not a ""lim"" source file")
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

        'Nothing
        If tokens.Count = 0 Then
            Exit Sub
        End If

        'Import std
        If Not Me.path.EndsWith("vb/libs/std.limlib") Then
            importFile(templateFolder & "/vb/libs/std.limlib")
        End If

        'Handle import statement
        Dim needDrawFrameFunction As Boolean = False
        While True

            'Tokens count
            If Not tokens.Count > 2 Then
                Exit While
            End If

            'New line
            If Not tokens(0).type = tokenType.CT_LINESTART Then
                Exit While
            End If

            'Check if line is import
            If Not tokens(1).type = tokenType.KW_IMPORT Then
                Exit While
            End If

            'Remove tokens
            tokens.RemoveAt(0)
            tokens.RemoveAt(0)

            'Handle filename
            If tokens(0).type = tokenType.CT_STRING Then

                'Import file
                importFile(tokens(0).value)

            ElseIf tokens(0).type = tokenType.CT_TEXT Then

                'Lower()
                tokens(0).value = tokens(0).value.ToString().ToLower()

                'Check file
                If Not File.Exists(templateFolder & "/vb/libs/" & tokens(0).value & ".limlib") Then
                    addBasicError("File not found", "the """ & tokens(0).value & """ library does not exist or has not been installed.")
                End If

                'Graphics
                If tokens(0).value = "graphics" Then
                    needDrawFrameFunction = True
                End If

                'Import file
                importFile(templateFolder & "/vb/libs/" & tokens(0).value & ".limlib")

            Else

                addSyntaxError("SFN01", "The keyword ""import"" is followed by the link to the file that you want to import.", Me, tokens(0).positionStart, tokens(0).positionEnd, "import ""my_file.lim""")
            End If

            'Remove tokens
            tokens.RemoveAt(0)

        End While

        'Generate AST (Abstract syntax tree)
        Dim parser As New AST()
        parser.parse(tokens, Me)

        'Get function
        For Each fun As FunctionNode In Me.functions

            If fun.Name = "drawFrame" Then

                'Set function
                Me.compiler.graphicsDrawFunction = fun

                'Handle type
                If Not fun.unsafeReturnType Is Nothing Then
                    addBasicWarning("Graphics lib", "The ""drawFrame"" function returns no value.")
                End If

                'Arguments
                If fun.Arguments.Count = 0 Then
                    addNodeSyntaxError("SFN01", "The ""drawFrame"" function must take an argument of type <image>.", fun, "func drawFrame(screen:image)")
                End If

                'Too many arguments
                If fun.Arguments.Count > 1 Then
                    addNodeSyntaxError("SFN02", "The ""drawFrame"" function must take an argument of type <image>.", fun, "func drawFrame(screen:image)")
                End If

                'Argument type
                If Not fun.Arguments(0).type.className = "image" Then
                    addNodeSyntaxError("SFN03", "The ""drawFrame"" function must take an argument of type <image>, the type of the argument indicate is <" & fun.Arguments(0).type.ToString() & ">.", fun, "func drawFrame(screen:image)")
                End If
                If Not fun.Arguments(0).type.Dimensions.Count = 0 Then
                    addNodeSyntaxError("SFN04", "The ""drawFrame"" function must take an argument of type <image>, the type of the argument indicate is <" & fun.Arguments(0).type.ToString() & ">.", fun, "func drawFrame(screen:image)")
                End If
                If Not fun.Arguments(0).declareType = VariableDeclarationType._let_ Then
                    addNodeSyntaxError("SFN05", "The ""drawFrame"" function must take an argument in reference", fun, "func drawFrame(screen:image)")
                End If

            End If

        Next

        'Set export
        Dim has_any_export As Boolean = False
        For Each declareVariable As DeclareVariableNode In Me.declareVariables
            If declareVariable.export Then
                has_any_export = True
            End If
        Next
        For Each functions As FunctionNode In Me.functions
            If functions.export Then
                has_any_export = True
            End If
        Next
        For Each anyClass As ClassNode In Me.classs
            If anyClass.export Then
                has_any_export = True
            End If
        Next

        'Export all ?
        If has_any_export = False Then
            For Each declareVariable As DeclareVariableNode In Me.declareVariables
                declareVariable.export = True
            Next
            For Each functions As FunctionNode In Me.functions
                functions.export = True
            Next
            For Each currentClass As ClassNode In Me.classs
                currentClass.export = True
            Next
        End If

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String

        'Variable
        Dim strVariables As String = ""
        Dim strFunctions As String = ""
        Dim strClasss As String = ""

        'Loops
        For Each code As Node In Me.declareVariables
            strVariables &= ", " & code.ToString()
        Next
        For Each code As Node In Me.functions
            strFunctions &= ", " & code.ToString()
        Next
        For Each code As Node In Me.classs
            strClasss &= ", " & code.ToString()
        Next

        'Fix
        If strVariables.StartsWith(", ") Then
            strVariables = "PROPRETIES: " & strVariables.Substring(2)
        End If
        If strFunctions.StartsWith(", ") Then
            strFunctions = "FUNCTIONS: " & strFunctions.Substring(2)
        End If
        If strClasss.StartsWith(", ") Then
            strClasss = "CLASSS: " & strClasss.Substring(2)
        End If

        'Return
        Return "(" & name & "{" & strVariables & " " & strFunctions & " " & strClasss & "}" & ")"

    End Function

    '=================================
    '========== IMPORT FILE ==========
    '=================================
    Public Sub importFile(ByVal path As String)

        'Fix path
        If path.Contains("\") Then
            path = path.Replace("\", "/")
        End If

        'Search if file already imported
        For Each existingFile As LimFile In Me.compiler.files
            If existingFile.path = path Then
                Me.FilesImports.Add(existingFile)
                Exit Sub
            End If
        Next

        'Get file
        Dim file As New LimFile(path, Me.compiler)

        'Import file
        Me.compiler.files.Add(file)

        'Add to file
        Me.FilesImports.Add(file)

    End Sub

End Class