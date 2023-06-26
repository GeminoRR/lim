'=======================================
'========== BRACKET SELECTOR  ==========
'=======================================
'
' Represents a binary operation
'
Class BracketSelectorNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Target As ValueNode
    Public Index As ValueNode
    Private TargetedRelation As RelationNode = Nothing

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As BracketSelectorNode = Me.MemberwiseClone()
        Cloned.Target = Cloned.Target.Clone(Cloned)
        Cloned.Index = Cloned.Index.Clone(Cloned)
        Cloned.TargetedRelation = Nothing
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Target As ValueNode, ByVal Index As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Target = Target
        Me.Target.ParentNode = Me
        Me.Index = Index
        Me.Index.ParentNode = Me

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "(" & Target.ToString() & ")[" & Index.ToString() & "]"
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
        For Each Relation As RelationNode In Target.ReturnType.Relations
            If Relation.RelationOperator = RelationOperator.INDEX And Relation.RelationArguments(1).ArgumentType = Index.ReturnType Then
                TargetedRelation = Relation
                Exit Sub
            End If
        Next

        'Not find
        ThrowNodeTypeException("BSNGR01", "No relation found for this operation", Me)

    End Sub

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get relation
        GetRelation()

        'Return return type of relation
        Return TargetedRelation.CompiledName & "(GV, (" & Target.Compile(content) & "), (" & Index.Compile(content) & "))"

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
