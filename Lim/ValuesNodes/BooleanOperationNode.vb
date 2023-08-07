'=======================================
'========== BOOLEAN OPERATION ==========
'=======================================
'
' Represents a boolean operation
'
Class BooleanOperationNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Left As ValueNode
    Public Right As ValueNode
    Dim Op As Token

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As BooleanOperationNode = Me.MemberwiseClone()
        Cloned.Left = Cloned.Left.Clone(Cloned)
        Cloned.Right = Cloned.Right.Clone(Cloned)
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Left As ValueNode, ByVal Right As ValueNode, ByVal Op As Token)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Left = Left
        Me.Left.ParentNode = Me
        Me.Right = Right
        Me.Right.ParentNode = Me
        Me.Op = Op

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "(" & Left.ToString() & ") " & Op.ToString() & " (" & Right.ToString() & ")"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Error
        If Not Left.ReturnType = STD_bool Then
            ThrowNodeTypeException("BONC01", "The value is of type """ & Left.ReturnType.ToString() & """ instead of ""bool"".", Left)
        End If
        If Not Right.ReturnType = STD_bool Then
            ThrowNodeTypeException("BONC02", "The value is of type """ & Right.ReturnType.ToString() & """ instead of ""bool"".", Right)
        End If

        'Get operator
        Dim C_Operator = ""
        Select Case Me.Op.Type

            Case TokenType.OP_AND
                C_Operator = "&&"

            Case TokenType.OP_OR
                C_Operator = "||"

            Case Else
                Throw New NotImplementedException()

        End Select

        'Compile
        Return "new_bool(*(" & Left.Compile(content) & ") " & C_Operator & " *(" & Right.Compile(content) & "))"

    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return Me.Left.IsConstant And Me.Right.IsConstant
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type
        Return STD_bool
    End Function

End Class
