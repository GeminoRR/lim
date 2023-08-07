'==================================
'========== BOOLEAN NODE ==========
'==================================
'
' Represents a boolean constant
' (True / False)
'
Class IsNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public CheckedValue As ValueNode
    Public CheckedType As TypeNode

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As BooleanNode = Me.MemberwiseClone()
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal CheckedValue As ValueNode, ByVal CheckedType As TypeNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.CheckedValue = CheckedValue
        Me.CheckedValue.ParentNode = Me
        Me.CheckedType = CheckedType
        Me.CheckedType.ParentNode = Me

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "(" & CheckedValue.ToString() & " is " & CheckedType.ToString() & ")"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        If Not CheckedValue.ReturnType = STD_any Then
            ThrowNodeTypeException("INC01", "The ""is"" operator can only have an ""any"" value to its left.", CheckedValue, "In this case the value is of type """ & CheckedValue.ReturnType.ToString() & """. This operation would therefore always return """ & (CheckedValue.ReturnType = CheckedType.AssociateType).ToString().ToLower() & """.")
        End If

        Return "new_bool((" & CheckedValue.Compile(content) & ")->typeID == " & CheckedType.AssociateType.TypeID.ToString() & ")"

    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return False
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type
        Return STD_bool
    End Function

End Class
