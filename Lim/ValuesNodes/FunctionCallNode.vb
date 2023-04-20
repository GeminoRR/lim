'========================================
'========== FUNCTION CALL NODE ==========
'========================================
'
' Represents a call to a function
'
Class FunctionCallNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public TargetFunction As Node
    Public PassedArguments As New List(Of Node)

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal TargetFunction As Node)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.TargetFunction = TargetFunction
        Me.TargetFunction.ParentNode = Me

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Arguments_STR As String = ""
        For Each arg As Node In PassedArguments
            Arguments_STR &= ", (" & arg.ToString() & ")"
        Next
        If Arguments_STR.StartsWith(", ") Then
            Arguments_STR = Arguments_STR.Substring(2)
        End If
        Arguments_STR = "(" & Arguments_STR & ")"
        Return TargetFunction.ToString() & Arguments_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String
        Return ""
    End Function

End Class