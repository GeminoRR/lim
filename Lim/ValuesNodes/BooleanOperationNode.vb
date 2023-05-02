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
        Throw New NotImplementedException()
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
