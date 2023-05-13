'===================================
'========== UNARY OP NODE ==========
'===================================
'
' Represents a single operation
' Example: -4
'
Class UnaryOpNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Op As Token
    Public Target As ValueNode
    Private TargetedRelation As RelationNode = Nothing

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As UnaryOpNode = Me.MemberwiseClone()
        Cloned.Target = Cloned.Target.Clone(Cloned)
        Cloned.TargetedRelation = Nothing
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Op As Token, ByVal Target As Node)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Op = Op
        Me.Target = Target
        Me.Target.ParentNode = Me

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return Op.ToString & "(" & Target.ToString() & ")"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get relation
        GetRelation()

        'Compile
        Return TargetedRelation.CompiledName & "(" & Target.Compile(content) & ")"

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

        'Get relation
        GetRelation()

        'Return type
        Return TargetedRelation.ReturnType

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
            If Relation.RelationOperator = RelationOperator.UNARY_MINUS Then
                TargetedRelation = Relation
                Exit Sub
            End If
        Next

        'Not find
        ThrowNodeTypeException("UONGR01", "No relation found for this operation", Me)

    End Sub

End Class
