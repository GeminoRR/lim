'===================================
'========== FOREARCH NODE ==========
'===================================
'
' Represents a for loop in each element of a list
'
Class ForeachNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public VariableName As String
    Public ExplicitVariableType As TypeNode
    Public Target As ValueNode
    Public Codes As New List(Of StatementNode)

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ForeachNode = Me.MemberwiseClone()
        If Cloned.ExplicitVariableType IsNot Nothing Then
            Cloned.ExplicitVariableType = Cloned.ExplicitVariableType.Clone(Cloned)
        End If
        Cloned.Target = Cloned.Target.Clone(Cloned)
        For i As Integer = 0 To Cloned.Codes.Count - 1
            Cloned.Codes(i) = Cloned.Codes(i).Clone(Cloned)
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal VariableName As String, ByVal ExplicitVariableType As TypeNode, ByVal Target As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.VariableName = VariableName
        Me.ExplicitVariableType = ExplicitVariableType
        If Me.ExplicitVariableType IsNot Nothing Then
            Me.ExplicitVariableType.ParentNode = Me
        End If
        Me.Target = Target
        Me.Target.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim ExplicitVariableType_STR As String = ""
        If ExplicitVariableType IsNot Nothing Then
            ExplicitVariableType_STR = ":" & ExplicitVariableType_STR.ToString()
        End If

        Return "(for " & VariableName & ExplicitVariableType_STR & " in (" & Target.ToString() & "))"

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'If target is iterable
        If Target.ReturnType.Iterators Is Nothing Then
            ThrowNodeTypeException("FENC01", "The """ & Target.ReturnType.ToString() & """ type is not iterable.", Target, "Check that the class of type """ & Target.ReturnType.ToString() & """ has the three relations: ""for_from"", ""for_to"", ""for_iterate"".")
        End If

        'Error
        If Not Target.ReturnType.Iterators.Item1.ReturnType = STD_int Then
            ThrowNodeTypeException("FENC02", "This relation must return an integer (""int"").", Target.ReturnType.Iterators.Item1)
        End If
        If Not Target.ReturnType.Iterators.Item2.ReturnType = STD_int Then
            ThrowNodeTypeException("FENC03", "This relation must return an integer (""int"").", Target.ReturnType.Iterators.Item2)
        End If
        If Not Target.ReturnType.Iterators.Item3.RelationArguments(1).ArgumentType = STD_int Then
            ThrowNodeTypeException("FENC04", "This argument must be of type ""int"".", Target.ReturnType.Iterators.Item3.RelationArguments(1))
        End If

        'Compile for header
        Dim TempVar As String = GetVariableCompiledName()
        Dim CompiledTarget As String = GetVariableCompiledName()
        Dim Max As String = GetVariableCompiledName()
        Content.Add("")
        Content.Add(Target.ReturnType.CompiledName & " * " & CompiledTarget & " = " & Target.Compile(Content) & ";")
        Content.Add("int " & Max & " = (" & Target.ReturnType.Iterators.Item2.CompiledName & "(" & CompiledTarget & "))->value;")
        Content.Add("for (" & STD_int.CompiledName & " * " & TempVar & " = " & Target.ReturnType.Iterators.Item1.CompiledName & "(" & CompiledTarget & "); (" & TempVar & ")->value < " & Max & "; ++(" & TempVar & "->value)){")

        'Compile variable
        Dim LoopVariable As New Variable(VariableName, Target.ReturnType.Iterators.Item3.ReturnType)
        Me.Variables.Add(LoopVariable)
        Content.Add(vbTab & LoopVariable.ValueType.CompiledName & " * " & LoopVariable.CompiledName & " = " & Target.ReturnType.Iterators.Item3.CompiledName & "(" & CompiledTarget & ", " & TempVar & ");")
        If ExplicitVariableType IsNot Nothing Then
            If Not Target.ReturnType.Iterators.Item3.ReturnType = ExplicitVariableType.AssociateType Then
                ThrowNodeTypeException("FENC04", "The """ & Target.ReturnType.ToString() & """ type iterator returns values of type """ & Target.ReturnType.Iterators.Item3.ReturnType.ToString() & """, but the explicit type is """ & ExplicitVariableType.AssociateType.ToString() & """.", ExplicitVariableType)
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
