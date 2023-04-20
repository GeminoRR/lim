'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Public Class Type

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly CompiledName As String

    '====================================
    '========== EQUAL OPERATOR ==========
    '====================================
    Shared Operator =(ByVal a As Type, ByVal b As Type)
        Return a.CompiledName = b.CompiledName
    End Operator

    Shared Operator <>(ByVal a As Type, ByVal b As Type)
        Return Not a = b
    End Operator


End Class
