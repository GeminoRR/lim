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

    Public variables As New List(Of DeclareVariableNode)
    Public functions As New List(Of FunctionNode)
    Public classs As New List(Of ClassNode)

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
        Dim parser As New AST()
        parser.parse(tokens, Me)

        Console.WriteLine(Me.ToString())
        endApp()

    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Variable
        Dim strVariables As String = ""
        Dim strFunctions As String = ""
        Dim strClasss As String = ""

        'Loops
        For Each code As Node In Me.variables
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

End Class