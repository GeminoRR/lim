'================================
'========== CLASS NODE ==========
'================================
'
' Represents the declaration of a class
'
Public Class ClassNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================


    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ParentFile As SourceFile, ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)

        'Inherits
        MyBase.New(Nothing, ParentFile, PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties


    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "()"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(ByVal content As List(Of String)) As String

        'Return
        Return ""

    End Function

End Class
