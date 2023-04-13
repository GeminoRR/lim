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

    Public Function IsKeyword(ByVal name As String)

        If Not Me.Type = TokenType.KEYWORD Then
            Return False
        End If
        Return Me.Value = name

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

        CONSTANT_INTEGER
        CONSTANT_FLOAT
        CONSTANT_STRING
        CONSTANT_TRUE
        CONSTANT_FALSE
        CONSTANT_NULL

        CODE_LINEINDENTATION
        CODE_TERM
        KEYWORD

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

        OP_PLUS
        OP_MINUS
        OP_DIVISION
        OP_MULTIPLICATION

    End Enum

    Public LimKeyword As New List(Of String) From {
        "func",
        "let"
    }

End Module