'==============================
'========== FOR NODE ==========
'==============================
'
' Represents a for loop
'
Class ForNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public VariableName As String
    Public ExplicitVariableType As TypeNode
    Public FromValue As ValueNode
    Public ToValue As ValueNode
    Public Codes As New List(Of StatementNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ForNode = Me.MemberwiseClone()
        If Cloned.ExplicitVariableType IsNot Nothing Then
            Cloned.ExplicitVariableType = Cloned.ExplicitVariableType.Clone(Cloned)
        End If
        If Cloned.FromValue IsNot Nothing Then
            Cloned.FromValue = Cloned.FromValue.Clone(Cloned)
        End If
        Cloned.ToValue = Cloned.ToValue.Clone(Cloned)
        For i As Integer = 0 To Cloned.Codes.Count - 1
            Cloned.Codes(i) = Cloned.Codes(i).Clone(Cloned)
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal VariableName As String, ByVal ExplicitVariableType As TypeNode, ByVal FromValue As ValueNode, ByVal ToValue As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.VariableName = VariableName
        Me.ExplicitVariableType = ExplicitVariableType
        If Me.ExplicitVariableType IsNot Nothing Then
            Me.ExplicitVariableType.ParentNode = Me
        End If
        Me.FromValue = FromValue
        If Me.FromValue IsNot Nothing Then
            Me.FromValue.ParentNode = Me
        End If
        Me.ToValue = ToValue
        Me.ToValue.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim ExplicitVariableType_STR As String = ""
        If ExplicitVariableType IsNot Nothing Then
            ExplicitVariableType_STR = ":" & ExplicitVariableType_STR.ToString()
        End If

        If Me.FromValue Is Nothing Then
            Return "(for " & VariableName & ExplicitVariableType_STR & " from 0 to (" & ToValue.ToString() & "))"
        Else
            Return "(for " & VariableName & ExplicitVariableType_STR & " from (" & FromValue.ToString() & " ) to (" & ToValue.ToString() & "))"
        End If

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Error
        If Not ToValue.ReturnType = STD_int Then
            ThrowNodeTypeException("FNC01", "A value of type ""int"" was expected here.", ToValue)
        End If
        If FromValue IsNot Nothing Then
            If Not FromValue.ReturnType = STD_int Then
                ThrowNodeTypeException("FNC02", "A value of type ""int"" was expected here.", ToValue)
            End If
        End If

        'Compile for header
        Content.Add("")
        Dim Max As String = GetVariableCompiledName()
        Dim Iterator As String = GetVariableCompiledName()
        Content.Add("int " & Max & " = (" & ToValue.Compile(Content) & ")->value;")
        If FromValue Is Nothing Then
            Content.Add("for (" & STD_int.CompiledName & " * " & Iterator & " = new_int(0); " & Iterator & "->value < " & Max & "; ++(" & Iterator & "->value)){")
        Else
            Content.Add("for (" & STD_int.CompiledName & " * " & Iterator & " = (" & FromValue.Compile(Content) & "); " & Iterator & "->value < " & Max & "; ++(" & Iterator & "->value)){")
        End If

        'Add variable
        Dim IteratorVariable As New Variable(VariableName, STD_int, False, Iterator)
        Me.Variables.Add(IteratorVariable)

        'Error  type
        If ExplicitVariableType IsNot Nothing Then
            If Not ExplicitVariableType.AssociateType = STD_int Then
                ThrowNodeTypeException("FNC03", "The variable """ & VariableName & """ must be of type ""int"".", ExplicitVariableType)
            End If
        End If

        'Compile content
        Dim LoopContent As New List(Of String)
        For Each line As StatementNode In Me.Codes
            line.Compile(LoopContent)
        Next
        For Each line As String In LoopContent
            Content.Add(vbTab & line)
        Next

        'Compile end of loop
        Content.Add("}")

    End Sub

End Class
