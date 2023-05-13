'============================
'========== RETURN ==========
'============================
'
' Returns a value (in a function or a relation)
'
Class ReturnNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private Value As ValueNode

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ReturnNode = Me.MemberwiseClone()
        Cloned.Value = Cloned.Value.Clone(Cloned)
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Value As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Value = Value
        If Me.Value IsNot Nothing Then
            Me.Value.ParentNode = Me
        End If

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "return " & Me.Value.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Get parent function
        Dim Parent As Node = Me
        While Parent.ParentNode IsNot Nothing

            'Get parent
            Parent = Parent.ParentNode

            'Is Function
            If TypeOf Parent Is FunctionNode Then

                'Cast
                Dim CastedFunction As FunctionNode = DirectCast(Parent, FunctionNode)

                'Function hasn't explicit return type
                If CastedFunction.ReturnType Is Nothing Then

                    'Return statement doesn't have a value
                    If Value Is Nothing Then
                        Content.Add("")
                        Content.Add("return NULL;")
                        Exit Sub
                    End If

                    'Return statement have a value
                    CastedFunction.ReturnType = Value.ReturnType
                    Content.Add("")
                    Content.Add("return (" & Value.Compile(Content) & ");")
                    Exit Sub

                End If

                'Function has a return type but there is no value to be return
                If Value Is Nothing Then
                    ThrowNodeTypeException("RNC02", "The function returns a value of type (" & CastedFunction.ReturnType.ToString() & ") but no value is returned here.", Me)
                End If

                'Error for the two types
                If Not Me.Value.ReturnType = CastedFunction.ReturnType Then
                    ThrowNodeTypeException("RNC03", "The function returns a value of type (" & CastedFunction.ReturnType.ToString() & ") but the value returned here is of type (" & Me.Value.ReturnType.ToString() & ").", Me.Value)
                End If
                Content.Add("")
                Content.Add("return (" & Value.Compile(Content) & ");")
                Exit Sub

            End If

            'Is Relation
            If TypeOf Parent Is RelationNode Then

                'Cast
                Dim CastedRelation As RelationNode = DirectCast(Parent, RelationNode)

                'Relation hasn't explicit return type
                If CastedRelation.ReturnType Is Nothing Then

                    'Return statement doesn't have a value
                    If Value Is Nothing Then
                        Content.Add("")
                        Content.Add("return NULL;")
                        Exit Sub
                    End If

                    'Return statement have a value
                    CastedRelation.ReturnType = Value.ReturnType
                    Content.Add("")
                    Content.Add("return (" & Value.Compile(Content) & ");")
                    Exit Sub

                End If

                'Function has a return type but there is no value to be return
                If Value Is Nothing Then
                    ThrowNodeTypeException("RNC04", "The relation returns a value of type (" & CastedRelation.ReturnType.ToString() & ") but no value is returned here.", Me)
                End If

                'Error for the two types
                If Not Me.Value.ReturnType = CastedRelation.ReturnType Then
                    ThrowNodeTypeException("RNC05", "The relation returns a value of type (" & CastedRelation.ReturnType.ToString() & ") but the value returned here is of type (" & Me.Value.ReturnType.ToString() & ").", Me.Value)
                End If
                Content.Add("")
                Content.Add("return (" & Value.Compile(Content) & ");")
                Exit Sub

            End If

        End While

        'Not find
        ThrowNodeSyntaxException("RNC01", "Unable to find the function/relation linked to this statement.", Me)

    End Sub

End Class
