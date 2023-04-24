'========================================
'========== NUMERIC VALUE NODE ==========
'========================================
'
' Represents a character string
'
Class StringNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Value As String

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Value As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Value = Value

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "'" & Value & "'"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        Dim ProcessValue As String = Me.Value

        ProcessValue = ProcessValue.Replace("\", "\\")
        ProcessValue = ProcessValue.Replace("""", "\""")

        Return "new_str(""" & ProcessValue & """)"

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
        Return STD_str
    End Function

End Class
