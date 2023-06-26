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
    Private _currentCharLine As Integer
    Private _currentCharColumn As Integer
    Private TranslateY As Integer
    Private TranslateX As Integer
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
        _currentCharColumn += 1
        TotalCount += 1

        'Advance line ?
        While _currentCharColumn >= lines(_currentCharLine).Count

            _currentCharLine += 1
            _currentCharColumn = 0

            If _currentCharLine >= lines.Count Then
                _currentCharLine = lines.Count - 1
                _currentCharColumn = lines(_currentCharLine).Count
                currentChar = Nothing
                currentCharLine = _currentCharLine + TranslateY
                currentCharColumn = _currentCharColumn + TranslateX
                Exit Sub
            End If

        End While

        'End of the text ?
        currentCharLine = _currentCharLine + TranslateY
        currentCharColumn = _currentCharColumn + TranslateX
        currentChar = lines(_currentCharLine)(_currentCharColumn)

    End Sub

    '===============================
    '========== LEX LINES ==========
    '===============================
    Public Function LexLines(ByVal lines As List(Of String), ByVal file As SourceFile, Optional ByVal TranslateY As Integer = 0, Optional ByVal TranslateX As Integer = 0) As List(Of Token)

        'Variables
        Dim result As New List(Of Token) From {New Token(TokenType.CODE_LINEINDENTATION, file, 0, 0, 0, 0, 0)}
        Lexer.lines = lines
        _currentCharLine = 0
        _currentCharColumn = -1
        Lexer.TranslateY = TranslateY
        Lexer.TranslateX = TranslateX
        Dim TabIndentation As String = Nothing
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
                Dim PositionEndY As Integer
                Dim PositionEndX As Integer
                Dim ContainerDot As Boolean = False

                While (Digits & ".").Contains(currentChar)
                    If currentChar = "." Then
                        If ContainerDot Then
                            Exit While
                        End If
                        ContainerDot = True
                    End If
                    numberString &= currentChar
                    PositionEndY = currentCharLine
                    PositionEndX = currentCharColumn
                    advance()
                End While

                If numberString.EndsWith(".") Then
                    ContainerDot = False
                    numberString = numberString.Substring(0, numberString.Length - 1)
                    TotalCount -= 2
                    _currentCharColumn -= 2
                    advance()
                End If

                If ContainerDot Then
                    'Float
                    Try
                        result.Add(New Token(TokenType.CT_FLOAT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Convert.ToDouble(numberString.Replace(".", ","))))
                    Catch ex As Exception
                        ThrowCoordinatesSyntaxLimException("LLP01", "Could not convert number to float.", file, PositionStartY, PositionStartX, PositionEndY, PositionEndX)
                    End Try

                Else
                    'Integer
                    Try
                        result.Add(New Token(TokenType.CT_INTEGER, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Convert.ToInt64(numberString)))
                    Catch ex As Exception
                        ThrowCoordinatesSyntaxLimException("LLP02", "Could not convert number to integer.", file, PositionStartY, PositionStartX, PositionEndY, PositionEndX)
                    End Try

                End If

                Continue While

            End If

            'String
            If {"""", "'"}.Contains(currentChar) Then

                Dim EndCharacter As Char = currentChar
                Dim Value As String = ""
                Dim SpecialChar As Boolean = False
                advance()

                While Not ((currentChar = EndCharacter And Not SpecialChar) Or currentChar = Nothing)
                    If currentChar = "\" And Not SpecialChar And EndCharacter = "'" Then
                        SpecialChar = True
                    Else
                        Value &= currentChar
                        If SpecialChar Then
                            SpecialChar = False
                        End If
                    End If
                    advance()
                End While

                If currentChar = Nothing Then
                    ThrowCoordinatesSyntaxLimException("LLP03", "The character " & EndCharacter & " was expected to terminate the string.", file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn)
                End If

                result.Add(New Token(TokenType.CT_STRING, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn, Value))
                advance()

            End If

            'Keyword or Code term
            If AuthorizedNameCharacters.Contains(currentChar) Then

                Dim Keyword As String = ""
                While AuthorizedNameCharacters.Contains(currentChar)

                    Keyword &= currentChar
                    advance()

                End While
                Dim TrueKeyword As String = Keyword
                Keyword = Keyword.ToLower()

                'Constant / Keyword
                Select Case Keyword

                    Case "true"
                        result.Add(New Token(TokenType.CT_TRUE, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "false"
                        result.Add(New Token(TokenType.CT_FALSE, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "null"
                        result.Add(New Token(TokenType.CT_NULL, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))

                    Case "import"
                        result.Add(New Token(TokenType.KW_IMPORT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "class"
                        result.Add(New Token(TokenType.KW_CLASS, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "func"
                        result.Add(New Token(TokenType.KW_FUNC, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "export"
                        result.Add(New Token(TokenType.KW_EXPORT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "primary"
                        result.Add(New Token(TokenType.KW_PRIMARY, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "extend"
                        result.Add(New Token(TokenType.KW_EXTEND, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "relation"
                        result.Add(New Token(TokenType.KW_RELATION, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))

                    Case "let"
                        result.Add(New Token(TokenType.KW_LET, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "return"
                        result.Add(New Token(TokenType.KW_RETURN, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "while"
                        result.Add(New Token(TokenType.KW_WHILE, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "if"
                        result.Add(New Token(TokenType.KW_IF, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "elseif"
                        result.Add(New Token(TokenType.KW_ELSEIF, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "else"
                        result.Add(New Token(TokenType.KW_ELSE, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "for"
                        result.Add(New Token(TokenType.KW_FOR, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "from"
                        result.Add(New Token(TokenType.KW_FROM, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "to"
                        result.Add(New Token(TokenType.KW_TO, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))

                    Case "new"
                        result.Add(New Token(TokenType.KW_NEW, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "and"
                        result.Add(New Token(TokenType.OP_AND, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "or"
                        result.Add(New Token(TokenType.OP_OR, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "not"
                        result.Add(New Token(TokenType.OP_NOT, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "has"
                        result.Add(New Token(TokenType.OP_HAS, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))
                    Case "in"
                        result.Add(New Token(TokenType.KW_IN, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, Keyword))

                    Case Else
                        result.Add(New Token(TokenType.CODE_TERM, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn - 1, TrueKeyword))

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

            '$
            If currentChar = "$" Then

                result.Add(New Token(TokenType.CODE_DOLLAR, file, PositionStartY, PositionStartX, currentCharLine, currentCharColumn))
                advance()
                Continue While

            End If

            'Linefeed
            If currentChar = vbLf Then

                Dim tabCount As Integer = 0

                While {Environment.NewLine, vbLf, vbCr, vbCrLf}.Contains(currentChar)
                    advance()
                End While

                If TabIndentation = Nothing Then

                    tabCount = 1
                    If currentChar = vbTab Then
                        TabIndentation = vbTab
                    ElseIf currentChar = " " Then
                        TabIndentation = " "
                        advance()
                        While currentChar = " "
                            TabIndentation &= " "
                            advance()
                        End While
                    Else
                        tabCount = 0
                    End If

                Else

                    Dim CurrentTabIndentation As String = TabIndentation
                    While currentChar = TabIndentation(0)

                        advance()
                        CurrentTabIndentation = CurrentTabIndentation.Substring(1)
                        If CurrentTabIndentation = "" Then
                            tabCount += 1
                            CurrentTabIndentation = TabIndentation
                        End If

                    End While

                End If
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

        'Add empty line at the end
        If Not result(result.Count - 1).Type = TokenType.CODE_LINEINDENTATION Then
            result.Add(New Token(TokenType.CODE_LINEINDENTATION, file, 0, 0, 0, 0))
        End If

        'Finish mega debug
        If MegaDebug Then
            Console.Write(Environment.NewLine)
        End If

        'Return
        Return result

    End Function

End Module
