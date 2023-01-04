'=========================
'========= CLASS =========
'=========================
Public Class ClassNode
    Inherits Node

    'Variable
    Public Name As String
    Public compiledName As String = ""
    Public arguments As List(Of String)

    Public addSourceDirectly As New List(Of AddSourceNode)
    Public declareVariables As New List(Of DeclareVariableNode)
    Public methods As New List(Of FunctionNode)
    Public relations As New List(Of RelationNode)

    Public export As Boolean = False
    Public primary As Boolean = False

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String, ByVal arguments As List(Of String), Optional compiledName As String = "")

        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.arguments = arguments
        Me.compiledName = compiledName

    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Export
        Dim export As String = ""
        If Me.export Then
            export = "export "
        End If

        'Primary
        Dim primary As String = ""
        If Me.primary Then
            primary = "primary "
        End If

        'Arguments
        Dim argumentsSTR As String = ""
        For Each arg As String In Me.arguments
            argumentsSTR &= ", " & arg
        Next
        If argumentsSTR.StartsWith(", ") Then
            argumentsSTR = "<" & argumentsSTR.Substring(2) & ">"
        End If

        'Return
        Return String.Format("({0}{1}{2}{3})", export, primary, Me.Name, argumentsSTR)

    End Function

End Class