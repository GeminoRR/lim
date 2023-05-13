'==================================
'========== SET VARIABLE ==========
'==================================
'
' Represents the assignment of a value to a variable
'   foo = bar
'   foor[bar] = foobar
'   foo.bar = foobar
'
Class SetVariableNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private Target As ValueNode
    Private NewValue As ValueNode

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As SetVariableNode = Me.MemberwiseClone()
        Cloned.Target = Cloned.Target.Clone(Cloned)
        Cloned.NewValue = Cloned.NewValue.Clone(Cloned)
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ComparisonNode As ComparisonNode)

        'Inherits
        MyBase.New(ComparisonNode.PositionStartY, ComparisonNode.PositionStartX, ComparisonNode.PositionEndY, ComparisonNode.PositionEndX)

        'Properties
        Me.Target = ComparisonNode.Left
        Me.Target.ParentNode = Me
        Me.NewValue = ComparisonNode.Right
        Me.NewValue.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return Me.Target.ToString() & " = " & Me.NewValue.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Compile
        Content.Add("")
        If TypeOf Target Is VariableNode Then

            'Check type compatibility
            If Not Target.ReturnType = NewValue.ReturnType Then
                ThrowNodeTypeException("SVNC01", "The type of the value (" & NewValue.ReturnType.ToString() & ") does not match that of the variable (" & Target.ReturnType.ToString() & ").", Me.NewValue)
            End If

            'Compile
            Content.Add(DirectCast(Target, VariableNode).CompileRef(Content) & " = (" & NewValue.Compile(Content) & ");")

        ElseIf TypeOf Target Is ChildNode Then

            'Check type compatibility
            If Not Target.ReturnType = NewValue.ReturnType Then
                ThrowNodeTypeException("SVNC01", "The type of the value (" & NewValue.ReturnType.ToString() & ") does not match that of the variable (" & Target.ReturnType.ToString() & ").", Me.NewValue)
            End If

            'Compile
            Content.Add(DirectCast(Target, ChildNode).CompileRef(Content) & " = (" & NewValue.Compile(Content) & ");")

        ElseIf TypeOf Target Is BracketSelectorNode Then

            'Get relation
            Dim CastedTarget As BracketSelectorNode = DirectCast(Target, BracketSelectorNode)
            For Each Relation As RelationNode In CastedTarget.Target.ReturnType.Relations
                If Relation.RelationOperator = RelationOperator.INDEX_SET Then

                    'Check arguments
                    If Not (Relation.RelationArguments(1).ArgumentType = CastedTarget.Index.ReturnType And Relation.RelationArguments(2).ArgumentType = NewValue.ReturnType) Then
                        Continue For
                    End If

                    'Compile method
                    Relation.Compile(Nothing)

                    'Compile
                    Content.Add(Relation.CompiledName & "((" & CastedTarget.Target.Compile(Content) & "), (" & CastedTarget.Index.Compile(Content) & "), (" & NewValue.Compile(Content) & "));")

                    'Exit
                    Exit Sub

                End If
            Next

            'Not find
            ThrowNodeTypeException("SVNC03", "No relation found for this operation", Me)

        Else

            ThrowNodeSyntaxException("SVNC02", "Cannot assign a value to this.", Target)

        End If

    End Sub

End Class
