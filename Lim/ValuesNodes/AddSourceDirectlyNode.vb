'=========================================
'========== ADD SOURCE DIRECTLY ==========
'=========================================
'
' Represents the declaration of a variable
'
Class AddSourceDirectlyNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public OriginalValue As String
    Private ReturnTypeNode As TypeNode

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As AddSourceDirectlyNode = Me.MemberwiseClone()
        If Cloned.ReturnTypeNode IsNot Nothing Then
            Cloned.ReturnTypeNode = Cloned.ReturnTypeNode.Clone(Cloned)
        End If
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal OriginalValue As String, Optional ByVal ReturnTypeNode As TypeNode = Nothing)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.OriginalValue = OriginalValue
        Me.ReturnTypeNode = ReturnTypeNode
        If Me.ReturnTypeNode IsNot Nothing Then
            Me.ReturnTypeNode.ParentNode = Me
        End If

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Dim ReturnType_STR As String = ""
        If Me.ReturnTypeNode IsNot Nothing Then
            ReturnType_STR = ":" & Me.ReturnTypeNode.ToString()
        End If
        Return "$""" & Me.OriginalValue & """" & ReturnType_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(ByVal content As List(Of String)) As String

        'Parse
        Dim ProcessValue As String = ""
        Dim WorkingString As String = Me.OriginalValue
        Dim LastGet As String = ""
        Dim Gettings As Boolean = False
        While WorkingString.Length > 0

            If Gettings Then

                LastGet &= WorkingString(0)
                WorkingString = WorkingString.Substring(1)

                If LastGet.EndsWith("}}") Then

                    LastGet = LastGet.Substring(0, LastGet.Length - 2)

                    Dim Tokens As List(Of Token) = Lexer.LexLines({LastGet}.ToList(), Me.ParentFile, Me.PositionStartY, Me.PositionStartX)
                    Tokens.RemoveAt(0) 'Remove lineIndentation

                    Dim node As Node = New AST().ParseValue(Tokens, Me.ParentFile)
                    node.ParentNode = Me

                    If TypeOf node Is ValueNode Then
                        ProcessValue &= DirectCast(node, ValueNode).Compile(content)
                    ElseIf TypeOf node Is TypeNode Then
                        ProcessValue &= DirectCast(node, TypeNode).AssociateType.CompiledName
                    Else
                        Throw New NotImplementedException()
                    End If

                    Gettings = False
                    LastGet = ""
                End If

            Else

                LastGet &= WorkingString(0)
                WorkingString = WorkingString.Substring(1)

                If LastGet.EndsWith("{{") Then
                    ProcessValue &= LastGet.Substring(0, LastGet.Length - 2)
                    LastGet = ""
                    Gettings = True
                End If

            End If

        End While
        ProcessValue &= LastGet

        'Return
        Return ProcessValue

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
        If ReturnTypeNode Is Nothing Then
            ThrowNodeTypeException("ASDNNRT01", "This node does not return any values.", Me, "If this node should return a value, please specify it as follows: $""something"":int")
        End If
        Return ReturnTypeNode.AssociateType
    End Function

End Class
