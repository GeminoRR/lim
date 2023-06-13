'==================================
'========== SYSTEM TOKEN ==========
'==================================
'
' Manage the token class.
'
Class Token

    Public Value As Object
    Public Type As TokenType

    Public SourceFile As SourceFile
    Public PositionStartY As Integer
    Public PositionStartX As Integer
    Public PositionEndY As Integer
    Public PositionEndX As Integer

    Public Sub New(ByVal Type As TokenType, ByVal SourceFile As SourceFile, ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, Optional ByVal Value As Object = Nothing)

        Me.Value = Value
        Me.Type = Type

        Me.SourceFile = SourceFile
        Me.PositionStartY = PositionStartY
        Me.PositionStartX = PositionStartX
        Me.PositionEndY = PositionEndY
        Me.PositionEndX = PositionEndX

    End Sub

    Public Overrides Function ToString() As String
        If Me.Value Is Nothing Then
            Return "[" & Me.Type.ToString() & "]"
        Else
            Return "[" & Me.Type.ToString() & ", " & Me.Value.ToString() & "]"
        End If
    End Function

End Class



'================================
'========== TOKEN ENUM ==========
'================================
'
' List token values.
'
Module TokenEnum

    Public Enum TokenType

        'Constant (CT)
        CT_INTEGER
        CT_FLOAT
        CT_STRING
        CT_TRUE
        CT_FALSE
        CT_NULL

        'Code component (CODE)
        CODE_LINEINDENTATION
        CODE_TERM
        CODE_DOLLAR
        CODE_COLON
        CODE_POINT

        'Keywords
        KW_IMPORT
        KW_CLASS
        KW_FUNC
        KW_RELATION
        KW_EXPORT
        KW_PRIMARY
        KW_EXTEND
        KW_LET
        KW_NEW
        KW_RETURN
        KW_FOR
        KW_FROM
        KW_TO
        KW_WHILE
        KW_IF
        KW_ELSEIF
        KW_ELSE

        'Operators (OP)
        OP_LEFT_PARENTHESIS
        OP_RIGHT_PARENTHESIS
        OP_LEFT_BRACKET
        OP_RIGHT_BRACKET

        OP_COMMA

        OP_EQUAL
        OP_LESSTHAN
        OP_LESSTHANEQUAL
        OP_MORETHAN
        OP_MORETHANEQUAL
        OP_IN

        OP_PLUS
        OP_MINUS
        OP_DIVISION
        OP_MULTIPLICATION

        OP_AND
        OP_OR
        OP_NOT

    End Enum

End Module