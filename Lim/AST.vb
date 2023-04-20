'=========================
'========== AST ==========
'=========================
'
' This class generates an abstract syntax tree from a list of tokens.
'
Module AST

    '=============================
    '========= VARIABLES =========
    '=============================
    Dim Tokens As List(Of Token)
    Dim TokenIndex As Integer
    Dim CurrentToken As Token
    Dim ParentFile As SourceFile

    '===========================
    '========= ADVANCE =========
    '===========================
    Private Sub advance()
        TokenIndex += 1
        If TokenIndex < Tokens.Count Then
            CurrentToken = Tokens(TokenIndex)
        Else
            ThrowCoordinatesSyntaxLimException("ASTA01", "Something was expected here", ParentFile, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
    End Sub

    '==========================
    '========= RECEDE =========
    '==========================
    Private Sub recede(ByVal index As Integer)
        TokenIndex = index
        If TokenIndex >= 0 And TokenIndex < Tokens.Count Then
            CurrentToken = Tokens(TokenIndex)
        Else
            ThrowSimpleLimException("ASTR01", "Internal problem", "Unable to find the token linked to the current index.")
        End If
    End Sub

    '=========================
    '========= PARSE =========
    '=========================
    Public Sub Parse(ByVal Tokens As List(Of Token), ByVal ParentFile As SourceFile)

        'Empty
        If Not Tokens.Count > 0 Then
            Exit Sub
        End If

        'Reload informations
        AST.ParentFile = ParentFile
        AST.Tokens = Tokens
        TokenIndex = -1
        advance()

        While TokenIndex < Tokens.Count - 1

            Dim Result As Node = GetClass()

            If TypeOf Result Is ClassNode Then
                ParentFile.Classes.Add(Result)
            ElseIf TypeOf Result Is FunctionNode Then
                ParentFile.Functions.Add(Result)
            ElseIf TypeOf Result Is DeclareVariableNode Then
                ParentFile.DeclareVariables.Add(Result)
            ElseIf TypeOf Result Is AddSourceDirectlyNode Then
                ParentFile.AddSourceDirectlys.Add(Result)
            Else
                ThrowNodeSyntaxException("ASTP01", "This type of node has nothing to do here.", Result, "Check line indentation")
            End If

        End While

    End Sub


    '========================
    '========= TYPE =========
    '========================
    Private Function GetTypeNode() As TypeNode

        'Erreur
        If Not CurrentToken.Type = TokenType.CODE_TERM Then
            ThrowCoordinatesSyntaxLimException("ASTGT01", "A class name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim ResultNode As New TypeNode(CurrentToken.PositionStartX, CurrentToken.PositionStartY, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value)
        advance()

        'No arguments
        If Not CurrentToken.Type = TokenType.OP_LESSTHAN Then
            Return ResultNode
        End If
        advance()

        'Also no arguments
        If Not CurrentToken.Type = TokenType.OP_MORETHAN Then

            While True

                'Advance
                Dim ArgumentValue As TypeNode = GetTypeNode()
                ArgumentValue.ParentNode = ResultNode
                ResultNode.PassedArguments.Add(ArgumentValue)

                'Continue
                If CurrentToken.Type = TokenType.OP_COMMA Then
                    advance()
                    Continue While
                ElseIf CurrentToken.Type = TokenType.OP_MORETHAN Then
                    Exit While
                Else
                    ThrowCoordinatesSyntaxLimException("ASTGT02", "A comma or greater than sign is missing here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If


            End While

        End If

        'End
        ResultNode.PositionEndY = CurrentToken.PositionEndY
        ResultNode.PositionEndX = CurrentToken.PositionEndX
        advance()

        'Specially for function types
        'They are the only ones that can have two argument lists, the second is used to represent the type returned by the function.
        '   Like this: fun<int, int><float>
        '   This represents the function : func ~(~:int, ~:int):float
        'The type of the second list is insert at the beginning of the first list. If this does not exist then "Nothing" will be inserted.
        If ResultNode.ClassName = "fun" Then

            'Second Argument
            Dim SecondArgument As TypeNode = Nothing

            'Argument
            If CurrentToken.Type = TokenType.OP_LESSTHAN Then
                advance()
                SecondArgument = GetTypeNode()
                If Not CurrentToken.Type = TokenType.OP_MORETHAN Then
                    ThrowCoordinatesSyntaxLimException("ASTGT03", "A greater than sign is missing here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                ResultNode.PositionEndY = CurrentToken.PositionEndY
                ResultNode.PositionEndX = CurrentToken.PositionEndX
                advance()
            End If

            'Insert
            ResultNode.PassedArguments.Insert(0, SecondArgument)

        End If

        'Return
        Return ResultNode

    End Function


    '==========================
    '========= FACTOR =========
    '==========================
    Private Function GetFactor() As Node

        Dim tok As Token = CurrentToken

        If tok.Type = TokenType.CONSTANT_INTEGER Or tok.Type = TokenType.CONSTANT_FLOAT Then
            advance()
            Return New NumericValueNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, tok)

        ElseIf tok.Type = TokenType.CONSTANT_STRING Then
            advance()
            Return New StringNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, tok.Value)

        ElseIf tok.Type = TokenType.CONSTANT_TRUE Then
            advance()
            Return New BooleanNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, True)

        ElseIf tok.Type = TokenType.CONSTANT_FALSE Then
            advance()
            Return New BooleanNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, False)

        ElseIf tok.Type = TokenType.CODE_TERM Then
            advance()
            Return New VariableNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, tok.Value)

        ElseIf tok.Type = TokenType.CODE_DOLLAR Then
            advance()
            If Not CurrentToken.Type = TokenType.CONSTANT_STRING Then
                ThrowCoordinatesSyntaxLimException("ASTGF03", "The ""$"" sign must be followed by a string representing the code to be inserted.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, "It is important to be familiar with the Lim runtime to use this feature.")
            End If
            Dim ValueToken As Token = CurrentToken
            advance()
            If Not CurrentToken.Type = TokenType.CODE_COLON Then
                Return New AddSourceDirectlyNode(tok.PositionStartY, tok.PositionStartX, ValueToken.PositionEndY, ValueToken.PositionEndX, ValueToken.Value)
            End If
            advance()
            Return New AddSourceDirectlyNode(tok.PositionStartY, tok.PositionStartX, ValueToken.PositionEndY, ValueToken.PositionEndX, ValueToken.Value, GetTypeNode())

        ElseIf tok.Type = TokenType.OP_LEFT_PARENTHESIS Then
            advance()
            Dim node = GetTopValue()
            If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                ThrowCoordinatesSyntaxLimException("ASTGF02", "A closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            advance()
            Return node

        ElseIf tok.Type = TokenType.CODE_LINEINDENTATION Then
            ThrowCoordinatesSyntaxLimException("ASTGF04", "The previous line is not complete.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)

        End If

        ThrowCoordinatesSyntaxLimException("ASTGF01", "A value was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        Return Nothing

    End Function

    '=================================
    '========= FUNCTION CALL =========
    '=================================
    Private Function GetFunctionCall() As Node

        Dim Value As Node = GetFactor()

        While CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS

            'Create node
            Dim NewValue As New FunctionCallNode(Value.PositionStartY, Value.PositionStartX, 0, 0, Value)

            'Arguments
            advance()
            If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then

                'Get all arguments
                While True

                    'Get the node
                    Dim Argument As Node = GetTopValue()
                    Argument.ParentNode = Value
                    NewValue.PassedArguments.Add(Argument)

                    'Continue loop
                    If CurrentToken.Type = TokenType.OP_COMMA Then
                        advance()
                        Continue While
                    ElseIf CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                        Exit While
                    Else
                        ThrowCoordinatesSyntaxLimException("ASTGFC01", "A comma or closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If

                End While

            End If

            'Update Value
            NewValue.PositionEndY = CurrentToken.PositionEndY
            NewValue.PositionEndX = CurrentToken.PositionEndX
            advance()
            Value = NewValue

        End While

        Return Value

    End Function

    '============================
    '========= NEW NODE =========
    '============================
    Private Function GetNew() As Node
        Return GetFunctionCall()
    End Function

    '====================================
    '========= BRACKET SELECTOR =========
    '====================================
    Private Function GetBracketSelector() As Node
        Return GetNew()
    End Function

    '===================================
    '========= UNARY OPERATION =========
    '===================================
    Private Function GetUnaryOperation() As Node

        If CurrentToken.Type = TokenType.OP_PLUS Or CurrentToken.Type = TokenType.OP_MINUS Then
            Dim Tok As Token = CurrentToken
            advance()
            Dim Target As Node = GetBracketSelector()
            Return New UnaryOpNode(CurrentToken.PositionStartY, CurrentToken.PositionStartX, Target.PositionEndY, Target.PositionEndX, Tok, Target)
        Else
            Return GetBracketSelector()
        End If

    End Function

    '========================
    '========= TERM =========
    '========================
    Private Function GetTerm() As Node
        Return GetUnaryOperation()
    End Function

    '========================
    '========= EXPR =========
    '========================
    Private Function GetExpr() As Node
        Return GetTerm()
    End Function

    '==============================
    '========= COMPARISON =========
    '==============================
    Private Function GetComparison() As Node
        Return GetExpr()
    End Function

    '=====================================
    '========= BOOLEAN OPERATION =========
    '=====================================
    Private Function GetBooleanOperation() As Node
        Return GetComparison()
    End Function

    '==================================
    '=========  GET TOP VALUE =========
    '==================================
    Private Function GetTopValue() As Node
        Return GetBooleanOperation()
    End Function

    '=============================
    '=========  GET LINE =========
    '=============================
    Private Function GetLine() As Node

        'Deep call
        Dim Value As Node = GetTopValue()

        'Function call
        If TypeOf Value Is FunctionCallNode Then
            Return Value
        End If

    End Function

    '=================================
    '=========  GET FUNCTION =========
    '=================================
    Private Function GetFunction() As Node

        'Save the state
        Dim SavedIndex As Integer = TokenIndex

        'Indentation
        If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
            ThrowCoordinatesSyntaxLimException("ASTGF01", "The start of a new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim IndentationToken As Token = CurrentToken
        Dim CurrentScopeIndentation As Integer = DirectCast(IndentationToken.Value, Integer)
        advance()

        'Export
        Dim export As Boolean = False
        If CurrentToken.Type = TokenType.KW_EXPORT Then
            export = True
            advance()
        End If

        'Is a function
        If Not CurrentToken.Type = TokenType.KW_FUNC Then
            recede(SavedIndex + 1)
            Return GetLine()
        End If
        advance()

        'Check indentation level
        If CurrentScopeIndentation > 1 Then
            ThrowCoordinatesSyntaxLimException("ASTGF02", "A function does not need an indentation while a method only needs an indentation. However, the indentation level is " & CurrentScopeIndentation.ToString() & " here.", ParentFile, IndentationToken.PositionStartY, IndentationToken.PositionStartX, IndentationToken.PositionEndY, IndentationToken.PositionEndX)
        End If

        'Function Name
        If Not CurrentToken.Type = TokenType.CODE_TERM Then
            ThrowCoordinatesSyntaxLimException("ASTGF03", "The function name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim ResultNode As New FunctionNode(Tokens(TokenIndex).PositionStartY, Tokens(TokenIndex).PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value, export)
        advance()

        'Arguments
        If CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS Then

            'No arguments
            advance()
            Dim OptionnalArguments As Boolean = False
            If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then

                While True

                    'Get argument name
                    If Not CurrentToken.Type = TokenType.CODE_TERM Then
                        ThrowCoordinatesSyntaxLimException("ASTGF04", "Argument name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim ArgumentNameToken As Token = CurrentToken
                    advance()

                    'Get argument type
                    Dim ArgumentType As TypeNode = Nothing
                    Dim ArgumentPositionEndY As Integer = 0
                    Dim ArgumentPositionEndX As Integer = 0
                    If CurrentToken.Type = TokenType.CODE_COLON Then
                        advance()
                        ArgumentType = GetTypeNode()
                        ArgumentPositionEndY = ArgumentType.PositionEndY
                        ArgumentPositionEndX = ArgumentType.PositionEndX
                    End If

                    'Value
                    Dim ArgumentValue As Node = Nothing
                    If CurrentToken.Type = TokenType.OP_EQUAL Then
                        advance()
                        ArgumentValue = GetTopValue()
                        OptionnalArguments = True
                        ArgumentPositionEndY = ArgumentValue.PositionEndY
                        ArgumentPositionEndX = ArgumentValue.PositionEndX
                    End If

                    'No Type & Value
                    If ArgumentType Is Nothing And ArgumentValue Is Nothing Then
                        ThrowCoordinatesSyntaxLimException("ASTGF06", "This argument has no associated type or value.", ParentFile, ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentNameToken.PositionEndY, ArgumentNameToken.PositionEndX, "Add the type of the argument or a default value if the argument is optional")
                    End If

                    'Only type but already optinonal defined
                    If ArgumentValue Is Nothing And OptionnalArguments Then
                        ThrowCoordinatesSyntaxLimException("ASTGF07", "It is not possible to define a non-optional argument after having already defined an optional argument before.", ParentFile, ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentNameToken.PositionEndY, ArgumentNameToken.PositionEndX)
                    End If

                    'Add argument
                    ResultNode.FunctionArguments.Add(New FunctionArgumentNode(ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentPositionEndY, ArgumentPositionEndX, ArgumentNameToken.Value, ArgumentType, ArgumentValue))

                    'Continue
                    If CurrentToken.Type = TokenType.OP_COMMA Then
                        advance()
                        Continue While
                    ElseIf CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                        Exit While
                    Else
                        ThrowCoordinatesSyntaxLimException("ASTGF05", "A comma or closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If

                End While

            End If
            advance()

        End If

        'Return type
        If CurrentToken.Type = TokenType.CODE_COLON Then
            advance()
            Dim FunctionReturnType As TypeNode = GetTypeNode()
            FunctionReturnType.ParentNode = ResultNode
            ResultNode.ReturnTypeNode = FunctionReturnType
        End If

        'Content
        While True

            'NewLine
            If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                ThrowCoordinatesSyntaxLimException("ASTGF08", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
            If LineIndentation <= CurrentScopeIndentation Then
                Exit While
            End If

            'Get content
            advance()
            Dim LineResult As Node = GetLine()
            LineResult.ParentNode = ResultNode
            ResultNode.Codes.Add(LineResult)

        End While

        'Return
        Return ResultNode

    End Function

    '=============================
    '========= GET CLASS =========
    '=============================
    Private Function GetClass() As Node

        'Save the state
        Dim SavedIndex As Integer = TokenIndex

        'Indentation
        If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
            ThrowCoordinatesSyntaxLimException("ASTGC01", "The start of a new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim IndentationToken As Token = CurrentToken
        Dim CurrentScopeIndentation As Integer = DirectCast(IndentationToken.Value, Integer)
        advance()

        'Export
        Dim export As Boolean = False
        If CurrentToken.Type = TokenType.KW_EXPORT Then
            export = True
            advance()
        End If

        'Primary
        Dim primary As Boolean = False
        If CurrentToken.Type = TokenType.KW_PRIMARY Then
            primary = True
            advance()
        End If

        'Is a class
        If Not CurrentToken.Type = TokenType.KW_CLASS Then
            recede(SavedIndex)
            Return GetFunction()
        End If
        advance()

        'Check indentation level
        If CurrentScopeIndentation > 1 Then
            ThrowCoordinatesSyntaxLimException("ASTGC02", "A class definition cannot be indented. However, the indentation level is " & CurrentScopeIndentation.ToString() & " here.", ParentFile, IndentationToken.PositionStartY, IndentationToken.PositionStartX, IndentationToken.PositionEndY, IndentationToken.PositionEndX)
        End If

        'Class Name
        If Not CurrentToken.Type = TokenType.CODE_TERM Then
            ThrowCoordinatesSyntaxLimException("ASTGC03", "The class name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim ResultNode As New ClassNode(Tokens(TokenIndex).PositionStartY, Tokens(TokenIndex).PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value, export, primary)
        advance()

        'Arguments
        If CurrentToken.Type = TokenType.OP_LESSTHAN Then

            'No arguments
            advance()
            If Not CurrentToken.Type = TokenType.OP_MORETHAN Then

                While True

                    'Get argument
                    If Not CurrentToken.Type = TokenType.CODE_TERM Then
                        ThrowCoordinatesSyntaxLimException("ASTGC04", "Argument name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    ResultNode.Arguments.Add(CurrentToken.Value)
                    advance()

                    'Continue
                    If CurrentToken.Type = TokenType.OP_COMMA Then
                        advance()
                        Continue While
                    ElseIf CurrentToken.Type = TokenType.OP_MORETHAN Then
                        Exit While
                    Else
                        ThrowCoordinatesSyntaxLimException("ASTGC05", "A comma or greater than sign is missing here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If

                End While

            End If
            advance()

        End If

        'Content
        While True

            'NewLine
            If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                ThrowCoordinatesSyntaxLimException("ASTGC06", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

            'Indentation
            Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
            If LineIndentation < 1 Then
                Exit While
            End If

            'Get content
            Dim LineResult As Node = GetFunction()
            LineResult.ParentNode = ResultNode
            If TypeOf LineResult Is FunctionNode Then
                ResultNode.Methods.Add(LineResult)
            ElseIf TypeOf LineResult Is DeclareVariableNode Then
                ResultNode.DeclareVariables.Add(LineResult)
            ElseIf TypeOf LineResult Is AddSourceDirectlyNode Then
                ResultNode.AddSourcesDirectly.Add(LineResult)
            Else
                ThrowNodeSyntaxException("ASTGC07", "This was not expected here.", LineResult, "Check line indentation.")
            End If


        End While

        'Return
        Return ResultNode

    End Function

End Module
