'===================================
'========== VARIABLE NODE ==========
'===================================
'
' Represents a variable call
'
Class VariableNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public VariableName As String

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal VariableName As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.VariableName = VariableName

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return VariableName
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