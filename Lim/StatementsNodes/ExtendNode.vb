'=================================
'========== EXTEND NODE ==========
'=================================
'
' Represents a extend block
'
Class ExtendNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ExtendTarget As String
    Public Content As Node


    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ExtendNode = Me.MemberwiseClone()
        Cloned.Content = Cloned.Content.Clone()
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Extendtarget As String, ByVal Content As Node)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.ExtendTarget = Extendtarget
        Me.Content = Content
        Me.Content.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Return "extend " & ExtendTarget & " " & Content.ToString()

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

    End Sub

End Class
