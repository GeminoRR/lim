'==========================
'========== NODE ==========
'==========================
'
' Represents a variable
'
Class Variable

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly VariableName As String
    Public ReadOnly CompiledName As String
    Public ReadOnly ValueType As Type
    Public ReadOnly Constant As Boolean
    Public ReadOnly GlobalVariable As Boolean
    Public Export As Boolean

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal VariableName As String, ByVal ValueType As Type, Optional ByVal Constant As Boolean = False, Optional CompiledName As String = Nothing, Optional Export As Boolean = False, Optional GlobalVariable As Boolean = False)
        Me.VariableName = VariableName
        Me.ValueType = ValueType
        Me.Constant = Constant
        Me.GlobalVariable = GlobalVariable
        If CompiledName = Nothing Then
            Me.CompiledName = GetVariableCompiledName()
        Else
            Me.CompiledName = CompiledName
        End If
        Me.Export = Export
    End Sub

End Class