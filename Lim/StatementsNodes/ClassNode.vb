﻿'================================
'========== CLASS NODE ==========
'================================
'
' Represents the declaration of a class
'
Class ClassNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ClassName As String
    Public Arguments As New List(Of String)
    Public Export As Boolean
    Public Primary As Boolean
    Public SoloType As String = Nothing

    Public DeclareVariables As New List(Of DeclareVariableNode)
    Public Methods As New List(Of FunctionNode)
    Public Relations As New List(Of RelationNode)
    Public AddSourcesDirectly As New List(Of AddSourceDirectlyStatementNode)
    Public ReadOnly ClassID As Integer

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Throw New NotImplementedException()

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal ClassName As String, ByVal Export As Boolean, ByVal Primary As Boolean)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        ClassIDCounter += 1
        Me.ClassID = ClassIDCounter
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

    '===========================
    '========== EQUAL ==========
    '===========================
    Shared Operator =(ByVal a As ClassNode, ByVal b As ClassNode)
        Return a.ClassID = b.ClassID
    End Operator
    Shared Operator <>(ByVal a As ClassNode, ByVal b As ClassNode)
        Return Not a.ClassID = b.ClassID
    End Operator

End Class
