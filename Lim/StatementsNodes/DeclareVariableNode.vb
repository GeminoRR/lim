'===========================================
'========== DECLARE VARIABLE NODE ==========
'===========================================
'
' Represents the declaration of a variable
'
' Can be used in :
'   - Main program
'   - Class
'   - Scope
'
Public Class DeclareVariableNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

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
