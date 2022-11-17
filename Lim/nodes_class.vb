'=========================
'========= CLASS =========
'=========================
Public Class ClassNode
    Inherits Node

    'Variable
    Public Name As String
    Public compiledName As String = ""
    Public compiled As Boolean

    Public variables As New List(Of Variable)
    Public declareVariables As New List(Of DeclareVariableNode)
    Public methods As New List(Of FunctionNode)

    Public addSourceDirectly As New List(Of AddSourceNode)

    Public export As Boolean = False

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String)

        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.compiled = False

        Dim clone_mehtod As FunctionNode = New FunctionNode(0, 0, "clone", New List(Of FunctionArgument), New typeNode(0, 0, Me.Name, New List(Of ValueType)))
        clone_mehtod.parentNode = Me
        Me.methods.Add(clone_mehtod)

    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Variable
        Dim varSTR As String = ""
        Dim funcSTR As String = ""

        'Loop
        For Each code As DeclareVariableNode In Me.declareVariables
            varSTR &= ", " & code.ToString()
        Next
        For Each code As FunctionNode In Me.methods
            funcSTR &= ", " & code.ToString()
        Next

        'Fix
        If varSTR.StartsWith(", ") Then
            varSTR = "PREPRETIES: " & varSTR.Substring(2)
        End If
        If funcSTR.StartsWith(", ") Then
            funcSTR = "METHODS: " & funcSTR.Substring(2)
        End If

        'Return
        Return "(" & Name & "{" & varSTR & funcSTR & Environment.NewLine & "})"

    End Function

End Class