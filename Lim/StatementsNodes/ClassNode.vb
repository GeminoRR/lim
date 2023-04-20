'================================
'========== CLASS NODE ==========
'================================
'
' Represents the declaration of a class
'
Class ClassNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ClassName As String
    Public Arguments As New List(Of String)
    Public Export As Boolean
    Public Primary As Boolean

    Public DeclareVariables As New List(Of DeclareVariableNode)
    Public Methods As New List(Of FunctionNode)
    Public AddSourcesDirectly As New List(Of AddSourceDirectlyNode)

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal ClassName As String, ByVal Export As Boolean, ByVal Primary As Boolean)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.ClassName = ClassName
        Me.Export = Export
        Me.Primary = Primary

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim Export_STR As String = ""
        If Export Then
            Export_STR = "export "
        End If

        Dim Primary_STR As String = ""
        If Primary Then
            Primary_STR = "primary "
        End If

        Dim Arguments_STR As String = ""
        If Arguments.Count > 0 Then
            For Each arg As String In Arguments
                Arguments_STR &= ", " & arg
            Next
            Arguments_STR = "<" & Arguments_STR.Substring(2) & ">"
        End If

        Return Export_STR & Primary_STR & "class " & ClassName & Arguments_STR

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(ByVal content As List(Of String)) As String

        'Return
        Return ""

    End Function

End Class
