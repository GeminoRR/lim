'================================
'========== COMPARISON ==========
'================================
'
' Represents a comparison
'
Class ComparisonNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Left As ValueNode
    Public Right As ValueNode
    Public Op As RelationOperator
    Private TargetedRelation As RelationNode = Nothing

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ComparisonNode = Me.MemberwiseClone()
        Cloned.Left = Cloned.Left.Clone(Cloned)
        Cloned.Right = Cloned.Right.Clone(Cloned)
        Cloned.TargetedRelation = Nothing
        Return Cloned

    End Function

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
        Select Case Op.Type

            Case TokenType.OP_EQUAL
                Me.Op = RelationOperator.EQUAL

            Case TokenType.OP_LESSTHAN
                Me.Op = RelationOperator.LESSTHAN

            Case TokenType.OP_LESSTHANEQUAL
                Me.Op = RelationOperator.LESSTHANEQUAL

            Case TokenType.OP_MORETHAN
                Me.Op = RelationOperator.MORETHAN

            Case TokenType.OP_MORETHANEQUAL
                Me.Op = RelationOperator.MORETHANEQUAL

            Case Else
                Throw New NotImplementedException()

        End Select

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "(" & Left.ToString() & ") " & Op.ToString() & " (" & Right.ToString() & ")"
    End Function

    '==================================
    '========== GET RELATION ==========
    '==================================
    Private Sub GetRelation()

        'Already check
        If TargetedRelation IsNot Nothing Then
            Exit Sub
        End If

        'Find
        For Each Relation As RelationNode In Left.ReturnType.Relations
            If Relation.RelationOperator = Me.Op And Relation.RelationArguments(1).ArgumentType = Right.ReturnType Then
                TargetedRelation = Relation
                Exit Sub
            End If
        Next

        'Not find
        ThrowNodeTypeException("CNGR", "No relation found for this operation", Me)

    End Sub

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get relation
        GetRelation()

        'Return return type of relation
        Return TargetedRelation.CompiledName & "((" & Left.Compile(content) & "), (" & Right.Compile(content) & "))"

    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return False
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type

        'Get relation
        GetRelation()

        'Return return type of relation
        Return TargetedRelation.ReturnType

    End Function

End Class
