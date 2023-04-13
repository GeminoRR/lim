'===========================
'========== LEXER ==========
'===========================
'
' Transforms a list of strings into a list of tokens.
'

Module Lexer

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private currentChar As Char
    Private currentCharLine As Integer
    Private currentCharColumn As Integer
    Private lines As List(Of String)

    Private Const AuthorizedNameCharacters As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    Private Const Digits As String = "1234567890"

    '=============================
    '========== ADVANCE ==========
    '=============================
    Private Sub advance()

        'Advance X
        currentCharColumn += 1

        'Advance line ?
        If currentCharColumn >= lines(currentCharLine).Count Then

            currentCharLine += 1
            currentCharColumn = 0

        End If

        'End of the text ?
        If currentCharLine >= lines.Count Then
            currentChar = Nothing
        Else
            currentChar = lines(currentCharLine)(currentCharColumn)
        End If

    End Sub

    '===========================
    '========== PARSE ==========
    '===========================
    Public Function LexerParse(ByVal lines As List(Of String), ByVal file As SourceFile) As List(Of Token)

        'Variables
        Dim result As New List(Of Token)
        Lexer.lines = lines
        currentCharLine = 0
        currentCharColumn = -1
        advance()

        'For each character of the text
        While Not currentChar = Nothing

            'Number
            If Digits.Contains(currentChar) Then

                Dim numberString As String = ""
                Dim PositionStartY = currentCharLine
                Dim PositionStartX = currentCharColumn

                While (Digits & ".").Contains(currentChar)
                    numberString &= currentChar
                    advance()
                End While

                If numberString.Contains(".") Then
                    'Float
                    Try
                        result.Add(New Token(TokenType.CONSTANT_FLOAT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Convert.ToDouble(numberString.Replace(".", ","))))
                    Catch ex As Exception
                        ThrowCoordinatesSyntaxLimException("LLP01", "Could not convert number to float.", file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn)
                    End Try

                Else
                    'Integer
                    Try
                        result.Add(New Token(TokenType.CONSTANT_INTEGER, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Convert.ToInt64(numberString)))
                    Catch ex As Exception
                        ThrowCoordinatesSyntaxLimException("LLP02", "Could not convert number to integer.", file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn)
                    End Try

                End If

            End If

        End While

        'Return
        Return result

    End Function

End Module
