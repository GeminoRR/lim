'=============================
'========== IF NODE ==========
'=============================
'
' Represents a if statement
'
Class IfNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public if_section As IfNodeSection
    Public elseif_sections As New List(Of IfNodeSection)
    Public else_section As IfNodeSection

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As IfNode = Me.MemberwiseClone()

        Cloned.if_section = Cloned.if_section.Clone(Cloned)

        Cloned.elseif_sections = New List(Of IfNodeSection)
        For i As Integer = 0 To Me.elseif_sections.Count - 1
            Cloned.elseif_sections.Add(Me.elseif_sections(i).Clone(Cloned))
        Next

        If Cloned.else_section IsNot Nothing Then
            Cloned.else_section = Cloned.else_section.Clone(Cloned)
        End If

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

        'Add new line
        Content.Add("")

        'Compile main if section
        if_section.Compile(Content)

        'Compile elseif
        For Each elseif_section In elseif_sections
            elseif_section.Compile(Content)
        Next

        'Compile else
        If else_section IsNot Nothing Then
            else_section.Compile(Content)
        End If

    End Sub

End Class

Class IfNodeSection
    Inherits ScopeNode

    Public Condition As ValueNode
    Public Content As New List(Of StatementNode)
    Public type As if_section_type

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal type As if_section_type)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Variables
        Me.type = type

    End Sub

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(content As List(Of String))

        'Main condition
        If Not Condition.ReturnType = STD_bool Then
            ThrowNodeTypeException("INS01", "A value of type ""bool"" was expected instead of """ & Condition.ReturnType.ToString() & """.", Condition)
        End If

        'Compile for header
        Select Case type
            Case if_section_type.section_if
                content.Add("if (*(" & Condition.Compile(content) & ")){")

            Case if_section_type.section_elseif
                content.Add("else if (*(" & Condition.Compile(content) & ")){")

            Case if_section_type.esction_else
                content.Add("else {")

            Case Else
                Throw New NotImplementedException
        End Select

        'Compile content
        Dim MainContent As New List(Of String)
        For Each line As StatementNode In Me.Content
            line.Compile(MainContent)
        Next
        For Each line As String In MainContent
            content.Add(vbTab & line)
        Next
        content.Add("}")

    End Sub

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As IfNodeSection = Me.MemberwiseClone()
        If Cloned.Condition IsNot Nothing Then
            Cloned.Condition = Cloned.Condition.Clone(Cloned)
        End If
        Cloned.Content = New List(Of StatementNode)
        For i As Integer = 0 To Me.Content.Count - 1
            Cloned.Content.Add(Me.Content(i).Clone(Cloned))
        Next
        Return Cloned

    End Function

End Class
Enum if_section_type
    section_if
    section_elseif
    esction_else
End Enum