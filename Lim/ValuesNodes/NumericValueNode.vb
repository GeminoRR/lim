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

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As NumericValueNode = Me.MemberwiseClone()
        Return Cloned

    End Function

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

        If Value.Type = TokenType.CT_INTEGER Then
            Return "new_int((int)" & Value.Value.ToString() & ")"
        Else
            Return "new_float((double)" & Value.Value.ToString().Replace(",", ".") & ")"
        End If

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

        If Value.Type = TokenType.CT_INTEGER Then
            Return STD_int
        Else
            Return STD_float
        End If

    End Function

End Class
