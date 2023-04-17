'=========================================
'========== ADD SOURCE DIRECTLY ==========
'=========================================
'
' Represents the declaration of a variable
'
Public Class AddSourceDirectlyNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Value As String

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ParentNode As Node, ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Value As String)

        'Inherits
        MyBase.New(ParentNode, PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Value = Value

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Dim ReturnType_STR As String = ""
        If Me.ReturnType Is Nothing Then
            ReturnType_STR = ":" & Me.ReturnType.ToString()
        End If
        Return "($""" & Me.Value & """" & ReturnType_STR & ")"
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
    Protected Overrides Function IsConstant() As Boolean
        Return True
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type
        ThrowNodeTypeException("ASDNNRT01", "This node does not return any values.", Me, "If this node should return a value, please specify it as follows: $""something"":int")
        Return Nothing
    End Function

End Class
