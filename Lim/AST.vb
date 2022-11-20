Public Class AST

    '=============================
    '========= VARIABLES =========
    '=============================
    Dim tokens As New List(Of token)
    Dim tok_index As Integer
    Dim current_tok As token
    Dim file As LimFile

    '===========================
    '========= ADVANCE =========
    '===========================
    Private Sub advance()
        tok_index += 1
        If tok_index < tokens.Count Then
            current_tok = tokens(tok_index)
        Else
            If current_tok Is Nothing Then
                addBasicError("NPA01", "Something was expected in <" & file.name & ">")
            Else
                addSyntaxError("NPA01", "Something was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
        End If
    End Sub

    '==========================
    '========= RECEDE =========
    '==========================
    Private Sub recede(ByVal index As Integer)
        tok_index = index
        If tok_index >= 0 And tok_index < tokens.Count Then
            current_tok = tokens(tok_index)
        Else
            addBasicError("Compileur problem", "Token undefined")
        End If
    End Sub

    '=========================
    '========= PARSE =========
    '=========================
    Public Sub parse(ByVal tokens As List(Of token), ByVal file As LimFile)

        'Reload informations
        Me.file = file
        Me.tokens.Clear()
        For Each tok As token In tokens
            Me.tokens.Add(tok)
        Next
        tok_index = -1
        advance()

        While tok_index < tokens.Count - 1

            Dim obj As Node = struct()
            obj.parentNode = file
            If TypeOf obj Is DeclareVariableNode Then
                file.declareVariables.Add(obj)
            ElseIf TypeOf obj Is FunctionNode Then
                file.functions.Add(obj)
            ElseIf TypeOf obj Is ClassNode Then
                file.classs.Add(obj)
            ElseIf TypeOf obj Is AddSourceNode And file.LimLib Then
                file.addSourceDirectly.Add(obj)
            Else
                addSyntaxError("ASTP01", "An element of type <" & obj.GetType().FullName & "> has nothing to do here.", file, obj.positionStart, obj.positionEnd)
            End If

        End While


        ''Get codes
        'While True

        '    If Not current_tok.type = tokenType.CT_LINESTART Then
        '        addCustomSyntaxError("NPS03", "A newline was expected here", filename, Text, current_tok.positionStart, current_tok.positionEnd)
        '    End If
        '    Dim currentLineIndentation As Integer = Convert.ToInt32(current_tok.value)

        '    If currentLineIndentation <= nameIndentation Then
        '        Exit While
        '    End If

        '    Dim toAdd As Node = Space()
        '    If TypeOf toAdd Is SpaceNode Or TypeOf toAdd Is FunctionNode Or TypeOf toAdd Is DeclareVariableNode Or TypeOf toAdd Is StructNode Then
        '        currentSpace.addNodeToCode(toAdd)
        '    Else
        '        addCustomSyntaxWarning("NPSW01", "The following line of code cannot be located in this frame, it will not be taken into account.", filename, Text, toAdd.positionStart, toAdd.positionEnd)
        '    End If

        'End While

    End Sub

    '========================
    '========= TYPE =========
    '========================
    Private Function type() As typeNode

        'Variables
        Dim waitTokenType As tokenType = Nothing

        'Handle error
        If Not current_tok.type = tokenType.CT_TEXT Then
            addSyntaxError("NPT01", "A name of a type was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Create unsafe type
        Dim currentType As New typeNode(current_tok.positionStart, current_tok.positionEnd, current_tok.value, New List(Of ValueType))
        advance()

        'Get dimensions
        While True

            If Not waitTokenType = Nothing Then
                If Not current_tok.type = waitTokenType Then
                    addSyntaxError("NPT02", "The token <" & waitTokenType.ToString() & "> was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                waitTokenType = Nothing
                advance()
                Continue While
            End If

            Select Case current_tok.type

                Case tokenType.OP_LBRACKET
                    waitTokenType = tokenType.OP_RBRACKET
                    currentType.Dimensions.Add(ValueType.list)
                    currentType.positionEnd = current_tok.positionEnd

                Case tokenType.OP_LBRACE
                    waitTokenType = tokenType.OP_RBRACE
                    currentType.Dimensions.Add(ValueType.map)
                    currentType.positionEnd = current_tok.positionEnd

                Case Else
                    Exit While

            End Select

            advance()

        End While

        'Return type
        Return currentType

    End Function

    '==========================
    '========= FACTOR =========
    '==========================
    Private Function factor() As Node

        Dim tok As token = current_tok

        If tok.type = tokenType.OP_PLUS Or tok.type = tokenType.OP_MINUS Then
            advance()
            Dim fac = factor()
            Return New UnaryOpNode(tok.positionStart, fac.positionEnd, tok, fac)

        ElseIf tok.type = tokenType.CT_INTEGER Or tok.type = tokenType.CT_FLOAT Then
            advance()
            Return New valueNode(tok.positionStart, tok.positionEnd, tok)

        ElseIf tok.type = tokenType.CT_STRING Then
            advance()
            Return New StringNode(tok.positionStart, tok.positionEnd, tok.value)

        ElseIf tok.type = tokenType.CT_TRUE Then
            advance()
            Return New BooleanNode(tok.positionStart, tok.positionEnd, True)

        ElseIf tok.type = tokenType.CT_FALSE Then
            advance()
            Return New BooleanNode(tok.positionStart, tok.positionEnd, False)

        ElseIf tok.type = tokenType.CT_TEXT Then
            advance()
            Return New VariableNode(tok.positionStart, tok.positionEnd, tok.value)

        ElseIf tok.type = tokenType.OP_LPAR Then
            advance()
            Dim com = comparison()
            If current_tok.type = tokenType.OP_RPAR Then
                advance()
                Return com
            Else
                addSyntaxError("NPF02", "A parenthesis was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If

        ElseIf tok.type = tokenType.OP_LBRACKET Then
            advance()
            Dim list As New ListNode(tok.positionStart, 0)

            'Empty
            If current_tok.type = tokenType.OP_RBRACKET Then
                addSyntaxError("NPF06", "You cannot use an empty list because its type cannot be determined.", file, tok.positionStart, current_tok.positionEnd)
                list.positionEnd = current_tok.positionEnd
                Return list
            End If

            While True

                'Get value
                list.addElement(comparison())

                'End list ?Q
                If current_tok.type = tokenType.OP_RBRACKET Then
                    list.positionEnd = current_tok.positionEnd
                    Exit While
                End If

                'Comma ?
                If current_tok.type = tokenType.OP_COMMA Then
                    advance()
                    Continue While
                End If

                'Error
                addSyntaxError("NPF03", "A comma or square bracket was expected here.", file, current_tok.positionStart, current_tok.positionEnd)

            End While
            advance()
            Return list

        ElseIf tok.type = tokenType.OP_LBRACE Then
            advance()
            Dim map As New MapNode(tok.positionStart, 0)
            While True

                'Get Key
                Dim key As Node = comparison()

                'Get value
                If Not current_tok.type = tokenType.OP_TWOPOINT Then
                    addSyntaxError("NPF04", "A "":"" was expected here.", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                advance()
                Dim value As Node = comparison()

                'Add key & value
                map.addElement(key, value)

                'End list ?
                If current_tok.type = tokenType.OP_RBRACE Then
                    map.positionEnd = current_tok.positionEnd
                    Exit While
                End If

                'Comma ?
                If current_tok.type = tokenType.OP_COMMA Then
                    advance()
                    Continue While
                End If

                'Error
                addSyntaxError("NPF05", "A comma or right brace was expected here.", file, current_tok.positionStart, current_tok.positionEnd)

            End While
            advance()
            Return map

        End If

        addSyntaxError("NPF01", "Something else was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        Return Nothing

    End Function

    '==============================
    '========= ADD SOURCE =========
    '==============================
    Private Function addSource() As Node

        'Other
        If Not current_tok.type = tokenType.OP_ADDSOURCE Then

            'Something else
            Return factor()

        End If

        'Get start position
        Dim positionStart As Integer = current_tok.positionStart

        'Get value
        advance()
        If Not current_tok.type = tokenType.CT_STRING Then
            addSyntaxError("ASTAS01", "A string was expected here.", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Create token
        Dim currentNode As New AddSourceNode(positionStart, current_tok.positionEnd, current_tok.value)
        advance()

        'Return
        Return currentNode

    End Function

    '================================
    '========= FunctionCall =========
    '================================
    Private Function functionCall() As Node

        'Start of function call
        If Not current_tok.type = tokenType.CT_TEXT Then

            'Something else
            Return addSource()

        End If

        'Get start position
        Dim startPosition As Integer = current_tok.positionStart
        Dim recedeIndex As Integer = tok_index

        'Get content
        Dim functionPath As String = current_tok.value
        advance()

        'Check if it's a function call node
        If Not current_tok.type = tokenType.OP_LPAR Then
            recede(recedeIndex)
            Return addSource()
        End If
        advance()

        'Get arguments
        Dim arguments As New List(Of Node)
        While True

            'End of function call
            If current_tok.type = tokenType.OP_RPAR Then
                Exit While
            End If

            'Get argument
            arguments.Add(comparison())

            'Comma
            If current_tok.type = tokenType.OP_COMMA Then
                advance()
            ElseIf Not current_tok.type = tokenType.OP_RPAR Then
                addSyntaxError("ASTFC01", "A comma or a closing parenthesis must have been omitted here.", file, current_tok.positionStart, current_tok.positionEnd)
            End If

        End While

        'Get positionEnd
        Dim endPosition As Integer = current_tok.positionEnd
        advance()

        'Add node
        Return New FunctionCallNode(startPosition, endPosition, functionPath, arguments)

    End Function

    '============================
    '========= New Node =========
    '============================
    Private Function newClass() As Node

        'Not new node
        If Not current_tok.type = tokenType.KW_NEW Then
            Return functionCall()
        End If

        'New node
        Dim startPosition As Integer = current_tok.positionStart
        advance()

        'Get function call
        Dim callFunction = functionCall()

        'Create node
        Return New newNode(startPosition, callFunction.positionEnd, callFunction)

    End Function

    '====================================
    '========= BracketsSelector =========
    '====================================
    Private Function BracketsSelector() As Node

        Dim Target As Node = newClass()
        While current_tok.type = tokenType.OP_LBRACKET
            advance()
            Dim Index As Node = newClass()
            Target = New BracketsSelectorNode(Target.positionStart, Index.positionEnd, Target, Index)
            If Not current_tok.type = tokenType.OP_RBRACKET Then
                addSyntaxError("NPBS01", "A ""]"" was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()
        End While

        Return Target

    End Function

    '==============================
    '========= Child Node =========
    '==============================
    Private Function child() As Node

        Dim left = BracketsSelector()
        While current_tok.type = tokenType.OP_POINT
            advance()
            Dim right = BracketsSelector()
            left = New childNode(left.positionStart, right.positionEnd, left, right)
        End While

        Return left

    End Function

    '========================
    '========= TERM =========
    '========================
    Private Function term() As Node

        Dim left = child()
        While current_tok.type = tokenType.OP_MULTIPLICATION Or current_tok.type = tokenType.OP_DIVISION Or current_tok.type = tokenType.OP_MODULO
            Dim op = current_tok
            advance()
            Dim right = child()
            left = New binOpNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '========================
    '========= EXPR =========
    '========================
    Private Function expr() As Node

        Dim left As Node = term()
        While current_tok.type = tokenType.OP_PLUS Or current_tok.type = tokenType.OP_MINUS
            Dim op As token = current_tok
            advance()
            Dim right As Node = term()
            left = New binOpNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '==============================
    '========= COMPARISON =========
    '==============================
    Private Function comparison() As Node

        Dim left As Node = expr()
        While {tokenType.OP_EQUAL, tokenType.OP_LESSTHAN, tokenType.OP_LESSTHANEQUAL, tokenType.OP_MORETHAN, tokenType.OP_MORETHANEQUAL, tokenType.OP_IN}.Contains(current_tok.type)
            Dim op As token = current_tok
            advance()
            Dim right As Node = expr()
            left = New ComparisonNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '========================
    '========= LINE =========
    '========================
    Private Function line(ByVal currentLineIndentation As Integer) As Node

        'It's another thing that a line
        If Not current_tok.type = tokenType.CT_LINESTART Then
            Return comparison()
        End If

        'Get startPosition
        Dim positionStart As Integer = current_tok.positionStart
        advance()

        'Return statement
        If current_tok.type = tokenType.KW_RETURN Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart

            'Get value
            advance()
            Dim value As Node = comparison()
            Return New ReturnNode(startPosition, current_tok.positionEnd, value)

        End If

        'While loop
        If current_tok.type = tokenType.KW_WHILE Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart
            advance()

            'Get condition
            Dim condition As Node = comparison()

            'Create while object
            Dim while_statement As New whileStatementNode(positionStart, current_tok.positionEnd, condition)

            'Get lines
            While True

                If Not current_tok.type = tokenType.CT_LINESTART Then
                    addSyntaxError("NPL05", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                If addLineIndentation <= currentLineIndentation Then
                    Exit While
                End If

                while_statement.addNodeToCode(line(addLineIndentation))

            End While

            'Set position end
            while_statement.positionEnd = tokens(tok_index - 1).positionEnd

            'Return
            Return while_statement

        End If

        'For loop
        If current_tok.type = tokenType.KW_FOR Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart
            advance()

            'Declaration
            Dim declarationType As VariableDeclarationType = VariableDeclarationType._let_
            If current_tok.type = tokenType.KW_VAR Then
                declarationType = VariableDeclarationType._var_
                advance()
            ElseIf current_tok.type = tokenType.KW_LET Then
                declarationType = VariableDeclarationType._let_
                advance()
            End If

            'Get variable name
            If Not current_tok.type = tokenType.CT_TEXT Then
                addSyntaxError("NPF06", "A variable name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            Dim variableName As String = current_tok.value
            advance()

            'Get in
            If Not current_tok.type = tokenType.OP_IN Then
                addSyntaxError("NPF07", "A ""in"" token was expected here", file, current_tok.positionStart, current_tok.positionEnd, "for variable_name in a_list")
            End If
            advance()

            'Get target
            Dim target As Node = comparison()

            'Create for object
            Dim for_statement As New forStatementNode(positionStart, current_tok.positionEnd, target, variableName, declarationType)

            'Get lines
            While True

                If Not current_tok.type = tokenType.CT_LINESTART Then
                    addSyntaxError("NPF08", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                If addLineIndentation <= currentLineIndentation Then
                    Exit While
                End If

                for_statement.addNodeToCode(line(addLineIndentation))

            End While

            'Set position end
            for_statement.positionEnd = tokens(tok_index - 1).positionEnd

            'Return
            Return for_statement

        End If

        'If statement
        If current_tok.type = tokenType.KW_IF Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart
            advance()

            'Get condition
            Dim condition As Node = comparison()

            'Get if lines
            Dim if_statements As New List(Of Node)
            While True

                If Not current_tok.type = tokenType.CT_LINESTART Then
                    addSyntaxError("NPF09", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                If addLineIndentation <= currentLineIndentation Then
                    Exit While
                End If

                if_statements.Add(line(addLineIndentation))

            End While

            'Else if
            Dim elseif_statement As New List(Of Tuple(Of Node, List(Of Node)))
            While current_tok.type = tokenType.CT_LINESTART

                'Advance
                advance()

                'Keyword
                If Not current_tok.type = tokenType.KW_ELSEIF Then
                    recede(tok_index - 1)
                    Exit While
                End If
                advance()

                'Get condition
                Dim elseif_condition As Node = comparison()

                'Get if lines
                Dim elseif_lines As New List(Of Node)
                While True

                    If Not current_tok.type = tokenType.CT_LINESTART Then
                        addSyntaxError("NPF10", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                    If addLineIndentation <= currentLineIndentation Then
                        Exit While
                    End If

                    elseif_lines.Add(line(addLineIndentation))

                End While

                'Add
                elseif_statement.Add(New Tuple(Of Node, List(Of Node))(elseif_condition, elseif_lines))

            End While

            'Else
            Dim recedeIndex As Integer = tok_index
            If Not current_tok.type = tokenType.CT_LINESTART Then
                addSyntaxError("NPL12", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()

            'Else
            Dim else_statement As New List(Of Node)
            If current_tok.type = tokenType.KW_ELSE Then

                'Advance
                advance()

                'Get if lines
                While True

                    If Not current_tok.type = tokenType.CT_LINESTART Then
                        addSyntaxError("NPF11", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                    If addLineIndentation <= currentLineIndentation Then
                        Exit While
                    End If

                    else_statement.Add(line(addLineIndentation))

                End While

            Else

                recede(tok_index)

            End If

            'Return
            Return New ifStatementNode(positionStart, tokens(tok_index - 1).positionEnd, condition, if_statements, elseif_statement, else_statement)

        End If

        'Declare variable
        If current_tok.type = tokenType.KW_VAR Or current_tok.type = tokenType.KW_LET Then

            'Get variable declaration type
            Dim declarationType As VariableDeclarationType = VariableDeclarationType._let_
            If current_tok.type = tokenType.KW_VAR Then
                declarationType = VariableDeclarationType._var_
            ElseIf current_tok.type = tokenType.KW_LET Then
                declarationType = VariableDeclarationType._let_
            End If
            advance()

            'Get name
            If Not current_tok.type = tokenType.CT_TEXT Then
                addSyntaxError("NPL02", "A variable name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            Dim variableName As String = current_tok.value
            advance()

            'Get type
            Dim variableUnsafeType As typeNode = Nothing
            If current_tok.type = tokenType.OP_TWOPOINT Then
                advance()
                variableUnsafeType = type()
            End If

            'Get endPosition
            Dim endPosition As Integer = current_tok.positionEnd

            'Get value
            Dim value As Node = Nothing
            If current_tok.type = tokenType.OP_EQUAL Then
                advance()
                value = comparison()
                endPosition = value.positionEnd
            End If

            'Handle type error
            If variableUnsafeType Is Nothing And value Is Nothing Then
                addSyntaxError("NPL03", "The declaration of a variable must be accompanied by either a type, a value, or both", file, positionStart, endPosition)
            End If

            'Add node
            Return New DeclareVariableNode(positionStart, endPosition, declarationType, variableName, value, variableUnsafeType)

        End If

        'Continue parsing
        Dim left As Node = comparison()

        'Set variable
        If TypeOf left Is ComparisonNode Then

            'Cast
            Dim castedNode As ComparisonNode = DirectCast(left, ComparisonNode)

            'Handle error
            If Not castedNode.op.type = tokenType.OP_EQUAL Then
                addSyntaxError("NPL04", "A comparison is irrelevant here", file, castedNode.positionStart, castedNode.positionEnd)
            End If

            'Add node
            Return New SetVariableNode(positionStart, castedNode.positionEnd, castedNode.leftNode, castedNode.rightNode)

        End If

        'Function call
        If TypeOf left Is FunctionCallNode Or TypeOf left Is childNode Then

            'Add node
            Return left

        End If

        'AddSource
        If TypeOf left Is AddSourceNode And file.LimLib Then

            Return left

        End If

        'Unknow line type
        addSyntaxError("NPL01", "Unable to find line type", file, positionStart, current_tok.positionEnd)
        Return Nothing

    End Function

    '========================
    '========= FUNC =========
    '========================
    Private Function func() As Node

        'Check indentation
        Dim needToRecede As Boolean = False
        Dim funcIndentation As Integer = 0
        If current_tok.type = tokenType.CT_LINESTART Then
            funcIndentation = Convert.ToInt32(current_tok.value)
            advance()
            needToRecede = True
        End If

        'Check if node is func
        If Not current_tok.type = tokenType.KW_FUNC Then

            'It's another thing that a function
            If needToRecede = True Then
                recede(tok_index - 1)
            End If
            Return line(funcIndentation)

        End If

        'Save start pos
        Dim startPosition As Integer = current_tok.positionStart
        advance()

        'Get error
        If Not current_tok.type = tokenType.CT_TEXT Then
            addSyntaxError("NPFU01", "A name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Get name
        Dim name As String = current_tok.value
        advance()

        'Get arguments
        Dim arguments As New List(Of FunctionArgument)
        If current_tok.type = tokenType.OP_LPAR Then 'func Name(username:str, var id:int[])

            'First arg
            advance()

            'Direct ending ?
            If current_tok.type = tokenType.OP_RPAR Then

                'Direct ending
                advance()

            Else

                'Arguments in there
                While True

                    'Variables
                    Dim LastArgumentName As String = ""
                    Dim LastArgumentDeclarationType As VariableDeclarationType = VariableDeclarationType._let_
                    Dim LastArgumentUnsafeType As typeNode = Nothing

                    'Search for a ref
                    If current_tok.type = tokenType.KW_VAR Then
                        LastArgumentDeclarationType = VariableDeclarationType._var_
                        advance()
                    End If

                    'Search for a name
                    If Not current_tok.type = tokenType.CT_TEXT Then
                        addSyntaxError("NPF02", "A argument name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    LastArgumentName = current_tok.value
                    advance()

                    'Search for a type
                    If Not current_tok.type = tokenType.OP_TWOPOINT Then
                        addSyntaxError("NPF03", "A argument type was expected here (example : ""my_argument:str[]"")", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    advance()
                    LastArgumentUnsafeType = type()

                    'Add argument
                    arguments.Add(New FunctionArgument(LastArgumentName, LastArgumentUnsafeType, LastArgumentDeclarationType))

                    'Search for end
                    If current_tok.type = tokenType.OP_COMMA Then

                        advance()

                    ElseIf current_tok.type = tokenType.OP_RPAR Then

                        advance()
                        Exit While

                    Else

                        addSyntaxError("NPF04", "An end of parenthesis or a comma was expected here", file, current_tok.positionStart, current_tok.positionEnd)

                    End If

                End While

            End If

        End If

        'Unsafe type
        Dim FunctionUnsafeType As typeNode = Nothing
        If current_tok.type = tokenType.OP_TWOPOINT Then

            advance()
            FunctionUnsafeType = type()

        End If

        'Create node
        Dim currentFunction As New FunctionNode(startPosition, startPosition + 1, name, arguments, FunctionUnsafeType)

        'Get error
        If Not current_tok.type = tokenType.CT_LINESTART Then
            addSyntaxError("NPF02", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Get codes
        While True

            If Not current_tok.type = tokenType.CT_LINESTART Then
                addSyntaxError("NPF05", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            Dim currentLineIndentation As Integer = Convert.ToInt32(current_tok.value)

            If currentLineIndentation <= funcIndentation Then
                Exit While
            End If

            currentFunction.addNodeToCode(line(currentLineIndentation))

        End While

        'Add node
        Return currentFunction

    End Function

    '==========================
    '========= STRUCT =========
    '==========================
    Private Function struct() As Node

        'Check indentation
        Dim needToRecede As Boolean = False
        Dim structIndentation As Integer = 0
        If current_tok.type = tokenType.CT_LINESTART Then
            structIndentation = Convert.ToInt32(current_tok.value)
            advance()
            needToRecede = True
        End If

        'Check if node is struct
        If Not current_tok.type = tokenType.KW_CLASS Then

            'It's another thing that a function
            If needToRecede = True Then
                recede(tok_index - 1)
            End If
            Return func()

        End If

        'Save start pos
        Dim startPosition As Integer = current_tok.positionStart
        advance()

        'Get error
        If Not current_tok.type = tokenType.CT_TEXT Then
            addSyntaxError("NPS01", "A name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Get name
        Dim currentStruct As New ClassNode(startPosition, startPosition + 1, current_tok.value)
        advance()

        'Get error
        If Not current_tok.type = tokenType.CT_LINESTART Then
            addSyntaxError("NPS02", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Get content
        While True

            If Not current_tok.type = tokenType.CT_LINESTART Then
                addSyntaxError("NPS03", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            Dim currentLineIndentation As Integer = Convert.ToInt32(current_tok.value)

            If currentLineIndentation <= structIndentation Then
                Exit While
            End If

            Dim toAdd As Node = func()
            toAdd.parentNode = currentStruct

            If TypeOf toAdd Is FunctionNode Then
                currentStruct.methods.Add(DirectCast(toAdd, FunctionNode))

            ElseIf TypeOf toAdd Is DeclareVariableNode Then
                currentStruct.declareVariables.Add(DirectCast(toAdd, DeclareVariableNode))

            ElseIf TypeOf toAdd Is AddSourceNode And file.LimLib Then
                currentStruct.addSourceDirectly.Add(DirectCast(toAdd, AddSourceNode))

            Else
                addSyntaxError("NPSW01", "The following line of code cannot be located in this frame, it will not be taken into account.", file, toAdd.positionStart, toAdd.positionEnd)

            End If

        End While

        'Add node
        Return currentStruct

    End Function

End Class