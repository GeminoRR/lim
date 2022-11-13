﻿'============================
'========= VARIABLE =========
'============================
Public Class Variable

    Public type As safeType
    Public name As String
    Public compiledName As String
    Public declarationType As VariableDeclarationType

    Public Sub New(ByVal name As String, ByVal type As safeType, ByVal compiledName As String, Optional declarationType As VariableDeclarationType = VariableDeclarationType._let_)

        Me.name = name
        Me.type = type
        Me.compiledName = compiledName
        Me.declarationType = declarationType

    End Sub

End Class