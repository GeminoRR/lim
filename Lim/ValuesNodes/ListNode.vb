'========================================
'========== NUMERIC VALUE NODE ==========
'========================================
'
' Represents a character string
'
Class ListNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Values As List(Of ValueNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ListNode = Me.MemberwiseClone()
        Cloned.Values = New List(Of ValueNode)
        For Each i As ValueNode In Me.Values
            Cloned.Values.Add(i.Clone(Cloned))
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Values As List(Of ValueNode))

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Values = Values
        For Each value As ValueNode In Me.Values
            value.ParentNode = Me
        Next

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Values_STR As String = ""
        For Each value As ValueNode In Me.Values
            Values_STR &= ", (" & value.ToString() & ")"
        Next
        If Values_STR.StartsWith(", ") Then
            Values_STR = Values_STR.Substring(2)
        End If
        Return "[" & Values_STR & "]"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get list type
        Dim ListType As Type = GetTypeFromClassAndArgs(Me, STDClass_list, {Me.Values(0).ReturnType}.ToList())

        'Get methods
        Dim Constructor As FunctionNode = Nothing
        Dim Add As FunctionNode = Nothing
        For Each method As FunctionNode In ListType.Methods
            If method.FunctionName = "new" Then
                Constructor = method
            ElseIf method.FunctionName = "add" Then
                Add = method
            End If
        Next
        If Constructor Is Nothing Then
            ThrowNodeException("LNC01", "Element missing from std.lim", "Could not find constructor of class """ & ListType.ToString() & """", Me)
        End If
        Constructor.Compile(Nothing)
        If Add Is Nothing Then
            ThrowNodeException("LNC02", "Element missing from std.lim", "Could not find the ""add"" method of the class """ & ListType.ToString() & """", Me)
        End If
        Add.Compile(Nothing)

        'Compile variable
        Dim TempVar As String = GetVariableCompiledName()
        content.Add(ListType.CompiledName & " * " & TempVar & " = " & Constructor.CompiledName & "(GV);")

        'Compile arguments
        For Each value As ValueNode In Me.Values
            content.Add(Add.CompiledName & "(GV, " & TempVar & ", (" & value.Compile(content) & "));")
        Next

        'Return
        Return TempVar

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
        Return GetTypeFromClassAndArgs(Me, STDClass_list, {Me.Values(0).ReturnType}.ToList())
    End Function

End Class
