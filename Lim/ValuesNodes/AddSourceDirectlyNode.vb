'=========================================
'========== ADD SOURCE DIRECTLY ==========
'=========================================
'
' Represents the declaration of a variable
'
Class AddSourceDirectlyNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Value As String
    Private ReturnTypeNode As TypeNode

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Value As String, Optional ByVal ReturnTypeNode As TypeNode = Nothing)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Value = Value
        Me.ReturnTypeNode = ReturnTypeNode
        If Me.ReturnTypeNode IsNot Nothing Then
            Me.ReturnTypeNode.ParentNode = Me
        End If

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Dim ReturnType_STR As String = ""
        If Me.ReturnType Is Nothing Then
            ReturnType_STR = ":" & Me.ReturnTypeNode.ToString()
        End If
        Return "$""" & Me.Value & """" & ReturnType_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(ByVal content As List(Of String)) As String

        'Return
        Return ""

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
        If ReturnTypeNode Is Nothing Then
            ThrowNodeTypeException("ASDNNRT01", "This node does not return any values.", Me, "If this node should return a value, please specify it as follows: $""something"":int")
        End If
        Return ReturnTypeNode.AssociateType
    End Function

End Class
