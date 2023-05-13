'===================================
'========== UNARY OP NODE ==========
'===================================
'
' Represents a single operation
' Example: -4
'
Class UnaryOpNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Op As Token
    Public Target As Node

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As UnaryOpNode = Me.MemberwiseClone()
        Cloned.Target = Cloned.Target.Clone(Cloned)
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Op As Token, ByVal Target As Node)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Op = Op
        Me.Target = Target
        Me.Target.ParentNode = Me

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return Op.ToString & "(" & Target.ToString() & ")"
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
