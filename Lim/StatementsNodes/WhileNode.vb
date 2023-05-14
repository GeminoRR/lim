'================================
'========== WHILE NODE ==========
'================================
'
' Represents a while loop
'
Class WhileNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Condition As ValueNode
    Public Codes As New List(Of StatementNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As WhileNode = Me.MemberwiseClone()
        Cloned.Condition = Cloned.Condition.Clone(Cloned)
        For i As Integer = 0 To Cloned.Codes.Count - 1
            Cloned.Codes(i) = Cloned.Codes(i).Clone(Cloned)
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Condition As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Condition = Condition
        Me.Condition.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "(while (" & Me.Condition.ToString() & "))"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Error
        If Not Condition.ReturnType = STD_bool Then
            ThrowNodeTypeException("WNC01", "A value of type ""bool"" was expected instead of """ & Condition.ReturnType.ToString() & """.", Condition)
        End If

        'Compile for header
        Content.Add("")
        Content.Add("while ((" & Condition.Compile(Content) & ")->value){")

        'Compile content
        Dim LoopContent As New List(Of String)
        For Each line As StatementNode In Me.Codes
            line.Compile(LoopContent)
        Next
        For Each line As String In LoopContent
            Content.Add(vbTab & line)
        Next

        'Compile end of loop
        Content.Add("}")

    End Sub

End Class
