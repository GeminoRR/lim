﻿'==========================
'========== NODE ==========
'==========================
'
' Node class.
'
Public MustInherit Class Node

    '===============================
    '========== VARIABLES ==========
    '===============================

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New()

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return ""
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public MustOverride Function compile() As String


    '=================================
    '========== GETNODETYPE ==========
    '=================================


End Class
