'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Public Class FunctionNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReturnType As Type


    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ParentNode As Node, ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)

        'Inherits
        MyBase.New(ParentNode, PositionStartY, PositionStartX, PositionEndY, PositionEndX)

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