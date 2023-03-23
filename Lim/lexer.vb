'===========================
'========== LEXER ==========
'===========================
Public Class lexer

    '=============================
    '========= VARIABLES =========
    '=============================
    Dim text As String

    Dim charCounter As Integer
    Dim currentChar As Char

    Private Const authorizedNameCharacters As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    Private Const digits As String = "1234567890"

    '===========================
    '========= ADVANCE =========
    '===========================
    Private Sub advance(Optional ByVal times As Integer = 1)

        For i As Integer = 0 To times - 1
            charCounter += 1

            If charCounter < text.Length Then
                currentChar = text(charCounter)
            Else
                currentChar = Nothing
            End If
        Next

    End Sub

    '=====================================
    '========= GetTokensFromLine =========
    '=====================================
    Public Function parse(ByVal file As LimFile) As List(Of token)

        Dim tokens As New List(Of token)
        If file.content = Nothing Then
            Return tokens
        End If
        Me.text = file.content
        charCounter = -1
        advance()

        Dim indentationCounter As Integer = 0

        tokens.Add(New token(tokenType.CT_LINESTART, 0, 0, "0"))
        While Not currentChar = Nothing

            If currentChar = """" Then
                'Create string token
                Dim posStart As Integer = charCounter
                advance()
                Dim create_string As String = ""
                While Not currentChar = """" And Not currentChar = Nothing
                    create_string &= currentChar
                    advance()
                End While
                tokens.Add(New token(tokenType.CT_STRING, posStart, charCounter, create_string))
                advance()

            ElseIf currentChar = "'" Then
                'Create string token
                Dim posStart As Integer = charCounter
                advance()
                Dim create_string As String = ""
                While Not currentChar = "'" And Not currentChar = Nothing
                    create_string &= currentChar
                    advance()
                End While
                tokens.Add(New token(tokenType.CT_STRING, posStart, charCounter, create_string))
                advance()

            ElseIf currentChar = "$" And file.limlib Then
                'Create addSource token
                tokens.Add(New token(tokenType.OP_ADDSOURCE, charCounter, charCounter))
                advance()

            ElseIf currentChar = "." Then
                'Point
                tokens.Add(New token(tokenType.OP_POINT, charCounter, charCounter))
                advance()

            ElseIf currentChar = "+" Then
                'Plus operator
                tokens.Add(New token(tokenType.OP_PLUS, charCounter, charCounter))
                advance()

            ElseIf currentChar = "-" Then
                'Minus operator
                tokens.Add(New token(tokenType.OP_MINUS, charCounter, charCounter))
                advance()

            ElseIf currentChar = "*" Then
                'Multiply operator
                tokens.Add(New token(tokenType.OP_MULTIPLICATION, charCounter, charCounter))
                advance()

            ElseIf currentChar = "/" Then
                'Divide operator
                If charCounter + 1 < text.Length Then
                    If text(charCounter + 1) = "/" Then
                        advance(2)
                        While Not (currentChar = vbCr Or currentChar = Environment.NewLine)
                            advance()
                        End While
                        Continue While
                    End If
                End If
                tokens.Add(New token(tokenType.OP_DIVISION, charCounter, charCounter))
                advance()

            ElseIf currentChar = "%" Then
                'Modulo operator
                tokens.Add(New token(tokenType.OP_MODULO, charCounter, charCounter))
                advance()

            ElseIf currentChar = "=" Then
                'Equal operator
                tokens.Add(New token(tokenType.OP_EQUAL, charCounter, charCounter))
                advance()

            ElseIf currentChar = ":" Then
                'TwoPoint operator
                tokens.Add(New token(tokenType.OP_TWOPOINT, charCounter, charCounter))
                advance()

            ElseIf currentChar = "@" Then
                'Multiply operator
                tokens.Add(New token(tokenType.OP_AT, charCounter, charCounter))
                advance()

            ElseIf currentChar = "!" Then
                'Not operator
                If charCounter + 1 < text.Length Then
                    If text(charCounter + 1) = "=" Then
                        tokens.Add(New token(tokenType.OP_NOTEQUAL, charCounter, charCounter + 1))
                        advance(2)
                        Continue While
                    End If
                End If
                tokens.Add(New token(tokenType.OP_NOT, charCounter, charCounter))
                advance()

            ElseIf currentChar = ">" Then
                'MoreThan operator
                If charCounter + 1 < text.Length Then
                    If text(charCounter + 1) = "=" Then
                        tokens.Add(New token(tokenType.OP_MORETHANEQUAL, charCounter, charCounter + 1))
                        advance(2)
                        Continue While
                    End If
                End If
                tokens.Add(New token(tokenType.OP_MORETHAN, charCounter, charCounter))
                advance()

            ElseIf currentChar = "<" Then
                'LessThan operator
                If charCounter + 1 < text.Length Then
                    If text(charCounter + 1) = "=" Then
                        tokens.Add(New token(tokenType.OP_LESSTHANEQUAL, charCounter, charCounter + 1))
                        advance(2)
                        Continue While
                    End If
                End If
                tokens.Add(New token(tokenType.OP_LESSTHAN, charCounter, charCounter))
                advance()

            ElseIf digits.Contains(currentChar) Then
                'Create number
                Dim create_number As String = currentChar
                Dim startPos As Integer = charCounter
                advance()
                Dim dot_count As Integer = 0
                While Not currentChar = Nothing And Not currentChar = " " And (digits & ".").Contains(currentChar)
                    If currentChar = "." Then
                        dot_count += 1
                    End If
                    create_number &= currentChar
                    advance()
                End While
                If dot_count = 0 Then
                    tokens.Add(New token(tokenType.CT_INTEGER, startPos, charCounter - 1, create_number))
                ElseIf dot_count = 1 Then
                    tokens.Add(New token(tokenType.CT_FLOAT, startPos, charCounter - 1, create_number))
                Else
                    addSyntaxError("LP02", "A number cannot contain more than one point", file, startPos, charCounter)
                End If

            ElseIf authorizedNameCharacters.Contains(currentChar) Then

                'Create the var
                Dim create_var As String = currentChar
                Dim startPos As Integer = charCounter
                advance()

                'Fstring
                If create_var = "f" And currentChar = """" Then
                    advance()
                    Dim create_string As String = ""
                    While Not currentChar = """" And Not currentChar = Nothing
                        create_string &= currentChar
                        advance()
                    End While
                    tokens.Add(New token(tokenType.OP_FSTRING, startPos, charCounter, create_string))
                    advance()
                    Continue While
                ElseIf create_var = "f" And currentChar = "'" Then
                    advance()
                    Dim create_string As String = ""
                    While Not currentChar = "'" And Not currentChar = Nothing
                        create_string &= currentChar
                        advance()
                    End While
                    tokens.Add(New token(tokenType.OP_FSTRING, startPos, charCounter, create_string))
                    advance()
                    Continue While
                End If

                'Normal values
                While Not currentChar = Nothing And (authorizedNameCharacters & digits).Contains(currentChar)

                    create_var &= currentChar
                    advance()

                End While

                Select Case create_var.ToLower()
                    Case "true"
                        tokens.Add(New token(tokenType.CT_TRUE, startPos, charCounter - 1))
                    Case "false"
                        tokens.Add(New token(tokenType.CT_FALSE, startPos, charCounter - 1))
                    Case "null"
                        tokens.Add(New token(tokenType.CT_NULL, startPos, charCounter - 1))
                    Case "new"
                        tokens.Add(New token(tokenType.KW_NEW, startPos, charCounter - 1))
                    Case "in"
                        tokens.Add(New token(tokenType.OP_IN, startPos, charCounter - 1))
                    Case "let"
                        tokens.Add(New token(tokenType.KW_LET, startPos, charCounter - 1))
                    Case "var"
                        tokens.Add(New token(tokenType.KW_VAR, startPos, charCounter - 1))
                    Case "return"
                        tokens.Add(New token(tokenType.KW_RETURN, startPos, charCounter - 1))
                    Case "class"
                        tokens.Add(New token(tokenType.KW_CLASS, startPos, charCounter - 1))
                    Case "func"
                        tokens.Add(New token(tokenType.KW_FUNC, startPos, charCounter - 1))
                    Case "import"
                        tokens.Add(New token(tokenType.KW_IMPORT, startPos, charCounter - 1))
                    Case "export"
                        tokens.Add(New token(tokenType.KW_EXPORT, startPos, charCounter - 1))
                    Case "while"
                        tokens.Add(New token(tokenType.KW_WHILE, startPos, charCounter - 1))
                    Case "for"
                        tokens.Add(New token(tokenType.KW_FOR, startPos, charCounter - 1))
                    Case "if"
                        tokens.Add(New token(tokenType.KW_IF, startPos, charCounter - 1))
                    Case "else"
                        tokens.Add(New token(tokenType.KW_ELSE, startPos, charCounter - 1))
                    Case "elseif"
                        tokens.Add(New token(tokenType.KW_ELSEIF, startPos, charCounter - 1))
                    Case "and"
                        tokens.Add(New token(tokenType.OP_AND, startPos, charCounter - 1))
                    Case "or"
                        tokens.Add(New token(tokenType.OP_OR, startPos, charCounter - 1))
                    Case "relation"
                        tokens.Add(New token(tokenType.KW_RELATION, startPos, charCounter - 1))
                    Case "primary"
                        tokens.Add(New token(tokenType.KW_PRIMARY, startPos, charCounter - 1))
                    Case "from"
                        tokens.Add(New token(tokenType.KW_FROM, startPos, charCounter - 1))
                    Case "to"
                        tokens.Add(New token(tokenType.KW_TO, startPos, charCounter - 1))
                    Case Else
                        tokens.Add(New token(tokenType.CT_TEXT, startPos, charCounter - 1, create_var))
                End Select

            ElseIf currentChar = " " Then
                'Pass
                advance()

            ElseIf currentChar = vbCr Then
                indentationCounter = 0
                tokens.Add(New token(tokenType.CT_LINESTART, charCounter, charCounter, indentationCounter))
                advance(2) 'Cariage Return + Line feed

            ElseIf currentChar = vbLf Then
                advance()

            ElseIf currentChar = Environment.NewLine Then
                indentationCounter = 0
                tokens.Add(New token(tokenType.CT_LINESTART, charCounter, charCounter, indentationCounter))
                While currentChar = Environment.NewLine
                    advance()
                End While

            ElseIf currentChar = vbTab Then
                'Get indentation
                If indentationCounter = -1 Then
                    advance()
                    Continue While
                End If
                Dim posStart As Integer = charCounter
                While currentChar = vbTab
                    indentationCounter += 1
                    advance()
                End While
                If tokens(tokens.Count - 1).type = tokenType.CT_LINESTART Then
                    tokens(tokens.Count - 1).positionStart = posStart
                    tokens(tokens.Count - 1).positionEnd = charCounter
                    tokens(tokens.Count - 1).value = indentationCounter.ToString()
                End If
                indentationCounter = -1


            ElseIf currentChar = "," Then
                'Separator
                tokens.Add(New token(tokenType.OP_COMMA, charCounter, charCounter))
                advance()

            ElseIf currentChar = "(" Then
                'lPar
                tokens.Add(New token(tokenType.OP_LPAR, charCounter, charCounter))
                advance()

            ElseIf currentChar = ")" Then
                'rPar
                tokens.Add(New token(tokenType.OP_RPAR, charCounter, charCounter))
                advance()

            ElseIf currentChar = "[" Then
                'lBracket
                tokens.Add(New token(tokenType.OP_LBRACKET, charCounter, charCounter))
                advance()

            ElseIf currentChar = "]" Then
                'rBracket
                tokens.Add(New token(tokenType.OP_RBRACKET, charCounter, charCounter))
                advance()

            ElseIf currentChar = "{" Then
                'Left Brace
                tokens.Add(New token(tokenType.OP_LBRACE, charCounter, charCounter))
                advance()

            ElseIf currentChar = "}" Then
                'Right Brace
                tokens.Add(New token(tokenType.OP_RBRACE, charCounter, charCounter))
                advance()

            Else
                'Error : Unauthorized character
                addSyntaxError("LP01", "The """ & currentChar & """ character was unexpected.", file, charCounter, charCounter)
            End If

        End While

        'Remove useless lines
        Dim i As Integer = 0
        While i < tokens.Count

            If tokens(i).type = tokenType.CT_LINESTART Then

                While i + 1 < tokens.Count
                    If tokens(i + 1).type = tokenType.CT_LINESTART Then
                        tokens.RemoveAt(i)
                    Else
                        Exit While
                    End If
                End While

            End If

            i += 1

        End While

        If Not tokens(tokens.Count - 1).type = tokenType.CT_LINESTART Then
            tokens.Add(New token(tokenType.CT_LINESTART, charCounter, charCounter, "0"))
        End If

        Return tokens


    End Function

End Class