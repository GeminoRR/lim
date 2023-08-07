﻿'=============================
'========== IF NODE ==========
'=============================
'
' Represents a if statement
'
Class IfNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public MainCondition As ValueNode
    Public MainCodes As New List(Of StatementNode)

    Public ElseIfs As List(Of Tuple(Of ValueNode, List(Of StatementNode)))

    Public ElseCodes As New List(Of StatementNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As IfNode = Me.MemberwiseClone()
        Cloned.MainCondition = Cloned.MainCondition.Clone(Cloned)
        Cloned.MainCodes = New List(Of StatementNode)
        For Each i As StatementNode In Me.MainCodes
            Cloned.MainCodes.Add(i.Clone(Cloned))
        Next
        Cloned.ElseIfs = New List(Of Tuple(Of ValueNode, List(Of StatementNode)))
        For Each i As Tuple(Of ValueNode, List(Of StatementNode)) In Me.ElseIfs
            Dim ClonedList As New List(Of StatementNode)
            For Each j As StatementNode In i.Item2
                ClonedList.Add(j.Clone(Me))
            Next
            Cloned.ElseIfs.Add((DirectCast(i.Item1.Clone(Cloned), ValueNode), ClonedList).ToTuple())
        Next
        Cloned.ElseCodes = New List(Of StatementNode)
        For Each i As StatementNode In Me.ElseCodes
            Cloned.ElseCodes.Add(i.Clone(Cloned))
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "(if | [{elseif}] | [else])"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Main condition
        If Not MainCondition.ReturnType = STD_bool Then
            ThrowNodeTypeException("INC01", "A value of type ""bool"" was expected instead of """ & MainCondition.ReturnType.ToString() & """.", MainCondition)
        End If

        'Compile for header
        Content.Add("")
        Content.Add("if (*(" & MainCondition.Compile(Content) & ")){")

        'Compile content
        Dim MainContent As New List(Of String)
        For Each line As StatementNode In Me.MainCodes
            line.Compile(MainContent)
        Next
        For Each line As String In MainContent
            Content.Add(vbTab & line)
        Next

        'Compile elseif
        For Each ElseIf_Statement As Tuple(Of ValueNode, List(Of StatementNode)) In ElseIfs

            'Error
            If Not ElseIf_Statement.Item1.ReturnType = STD_bool Then
                ThrowNodeTypeException("INC02", "A value of type ""bool"" was expected instead of """ & ElseIf_Statement.Item1.ReturnType.ToString() & """.", ElseIf_Statement.Item1)
            End If

            'Compile
            Content.Add(vbTab)
            Content.Add("} else if (*(" & ElseIf_Statement.Item1.Compile(Content) & ")){")

            'Compile content
            Dim ElseIfContent As New List(Of String)
            For Each line As StatementNode In ElseIf_Statement.Item2
                line.Compile(ElseIfContent)
            Next
            For Each line As String In ElseIfContent
                Content.Add(vbTab & line)
            Next

        Next

        'Else
        If ElseCodes.Count > 0 Then

            'Compile
            Content.Add(vbTab)
            Content.Add("} else {")

            'Compile content
            Dim ElseContent As New List(Of String)
            For Each line As StatementNode In ElseCodes
                line.Compile(ElseContent)
            Next
            For Each line As String In ElseContent
                Content.Add(vbTab & line)
            Next

        End If

        'Compile end of if statement
        Content.Add("}")

    End Sub

End Class