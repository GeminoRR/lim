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
    Private TotalCount As Integer
    Private lines As List(Of String)

    Private Const AuthorizedNameFirstLetterCharacters As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    Private Const Digits As String = "1234567890"
    Private Const AuthorizedNameCharacters As String = AuthorizedNameFirstLetterCharacters & Digits

    '=============================
    '========== ADVANCE ==========
    '=============================
    Private Sub advance()

        'Advance X
        currentCharColumn += 1
        TotalCount += 1

        'Advance line ?
        While currentCharColumn >= lines(currentCharLine).Count

            currentCharLine += 1
            currentCharColumn = 0

            If currentCharLine >= lines.Count Then
                currentCharLine = lines.Count - 1
                currentCharColumn = lines(currentCharLine).Count
                currentChar = Nothing
                Exit Sub
            End If

        End While

        'End of the text ?
        currentChar = lines(currentCharLine)(currentCharColumn)

    End Sub

    '===========================
    '========== PARSE ==========
    '===========================
    Public Function LexerParse(ByVal lines As List(Of String), ByVal file As SourceFile) As List(Of Token)

        'Variables
        Dim result As New List(Of Token) From {New Token(TokenType.CODE_LINEINDENTATION, file, 0, 0, 0, 0, 0)}
        Lexer.lines = lines
        currentCharLine = 0
        currentCharColumn = -1
        TotalCount = 0

        'Debug
        Dim MegaDebugProgressPos As Integer = 0
        Dim TotalCharacters As Integer = 0
        If MegaDebug Then
            For Each line As String In lines
                TotalCharacters += line.Length
            Next
            Dim message As String = "[LOG] """ & file.filename & """"
            Console.Write(message)
            MegaDebugProgressPos = message.Length + 1
        End If

        'No lines ?
        If lines.Count < 1 Then
            If MegaDebug Then
                Console.SetCursorPosition(MegaDebugProgressPos, Console.CursorTop)
                Console.Write("0/0")
                Console.Write(Environment.NewLine)
            End If
            Return result
        End If

        'For each character of the text
        advance()
        While Not currentChar = Nothing

            'Mega Debug
            If MegaDebug Then
                Console.SetCursorPosition(MegaDebugProgressPos, Console.CursorTop)
                Console.Write(TotalCount.ToString(StrDup(TotalCharacters.ToString().Length, "0")) & "/" & TotalCharacters.ToString())
            End If

            'Get current char start positions
            Dim PositionStartY = currentCharLine
            Dim PositionStartX = currentCharColumn

            'Number
            If Digits.Contains(currentChar) Then

                Dim numberString As String = ""

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

                Continue While

            End If

            'String
            If {"""", "'"}.Contains(currentChar) Then

                Dim EndCharacter As Char = currentChar
                Dim Value As String = ""
                advance()

                While Not (currentChar = EndCharacter Or currentChar = Nothing)
                    Value &= currentChar
                    advance()
                End While

                If currentChar = Nothing Then
                    ThrowCoordinatesSyntaxLimException("LLP03", "The character " & EndCharacter & " was expected to terminate the string.", file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn)
                End If

                result.Add(New Token(TokenType.CONSTANT_STRING, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Value))
                advance()

            End If

            'Keyword or Code term
            If AuthorizedNameCharacters.Contains(currentChar) Then

                Dim Keyword As String = ""
                While AuthorizedNameCharacters.Contains(currentChar)

                    Keyword &= currentChar
                    advance()

                End While
                Keyword = Keyword.ToLower()

                'Constant / Keyword
                Select Case Keyword

                    Case "true"
                        result.Add(New Token(TokenType.CONSTANT_TRUE, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Keyword))
                    Case "false"
                        result.Add(New Token(TokenType.CONSTANT_FALSE, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Keyword))
                    Case "null"
                        result.Add(New Token(TokenType.CONSTANT_NULL, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Keyword))

                    Case "import"
                        result.Add(New Token(TokenType.KW_IMPORT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Keyword))

                    Case Else
                        result.Add(New Token(TokenType.CODE_TERM, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Keyword))

                End Select

                'End
                Continue While

            End If

            'Space
            If currentChar = " " Or currentChar = vbTab Then

                advance()
                Continue While

            End If

            '(
            If currentChar = "(" Then

                result.Add(New Token(TokenType.OP_LEFT_PARENTHESIS, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            ')
            If currentChar = ")" Then

                result.Add(New Token(TokenType.OP_RIGHT_PARENTHESIS, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '[
            If currentChar = "[" Then

                result.Add(New Token(TokenType.OP_LEFT_BRACKET, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            ']
            If currentChar = "]" Then

                result.Add(New Token(TokenType.OP_RIGHT_BRACKET, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            ',
            If currentChar = "," Then

                result.Add(New Token(TokenType.OP_COMMA, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '=
            If currentChar = "=" Then

                result.Add(New Token(TokenType.OP_EQUAL, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '> | >=
            If currentChar = ">" Then

                advance()
                If currentChar = "=" Then
                    '>=
                    result.Add(New Token(TokenType.OP_MORETHANEQUAL, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                    advance()
                Else
                    '>
                    result.Add(New Token(TokenType.OP_MORETHAN, file, PositionStartY, PositionStartX, PositionStartY, PositionStartX))
                End If

                Continue While

            End If

            '< | <=
            If currentChar = "<" Then

                advance()
                If currentChar = "=" Then
                    '<=
                    result.Add(New Token(TokenType.OP_LESSTHANEQUAL, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                    advance()
                Else
                    '<
                    result.Add(New Token(TokenType.OP_LESSTHAN, file, PositionStartY, PositionStartX, PositionStartY, PositionStartX))
                End If

                Continue While

            End If

            '+
            If currentChar = "+" Then

                result.Add(New Token(TokenType.OP_PLUS, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '-
            If currentChar = "-" Then

                result.Add(New Token(TokenType.OP_MINUS, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '*
            If currentChar = "*" Then

                result.Add(New Token(TokenType.OP_MULTIPLICATION, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '/ | //
            If currentChar = "/" Then

                advance()
                If currentChar = "/" Then
                    '// Comment
                    While Not (currentChar = vbLf Or currentChar = Nothing)
                        advance()
                    End While
                Else
                    '/
                    result.Add(New Token(TokenType.OP_DIVISION, file, PositionStartY, PositionStartX, PositionStartY, PositionStartX))
                End If

                Continue While

            End If

            '$
            If currentChar = "$" Then

                result.Add(New Token(TokenType.CODE_DOLLAR, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            ':
            If currentChar = ":" Then

                result.Add(New Token(TokenType.CODE_COLON, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            '.
            If currentChar = "." Then

                result.Add(New Token(TokenType.CODE_POINT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            'Linefeed
            If currentChar = vbLf Then

                Dim tabCount As Integer = 0

                While {Environment.NewLine, vbLf, vbCr, vbCrLf}.Contains(currentChar)
                    advance()
                End While

                While currentChar = vbTab

                    advance()
                    tabCount += 1

                End While

                result.Add(New Token(TokenType.CODE_LINEINDENTATION, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, tabCount))

                Continue While

            End If

            'Nothing
            ThrowCoordinatesSyntaxLimException("LLP00", "The """ & currentChar & """ character was unexpected here.", file, currentCharLine, currentCharColumn, currentCharLine, currentCharColumn)

        End While

        'Remove empty lines
        Dim i As Integer = -1
        Dim LastTokenIsNewLine As Boolean = False
        While i + 1 < result.Count

            i += 1

            If Not result(i).Type = TokenType.CODE_LINEINDENTATION Then
                LastTokenIsNewLine = False
                Continue While
            End If

            If LastTokenIsNewLine Then
                i -= 1
                result.RemoveAt(i)
            Else
                LastTokenIsNewLine = True
            End If

        End While

        'Finish mega debug
        If MegaDebug Then
            Console.Write(Environment.NewLine)
        End If

        'Return
        Return result

    End Function

End Module
