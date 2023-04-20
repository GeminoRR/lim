'========================================
'========== NUMERIC VALUE NODE ==========
'========================================
'
' Represents a numeric value
' (Integer / Floating Point)
'
Class NumericValueNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Value As Token

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Value As Token)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Value = Value

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return Value.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String
        Throw New NotImplementedException()
    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return True
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type
        Throw New NotImplementedException()
    End Function

End Class
