'=========================
'========== AST ==========
'=========================
'
' This class generates an abstract syntax tree from a list of tokens.
'
Class AST

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

    '===============================
    '========= PARSE VALUE =========
    '===============================
    Public Function ParseValue(ByVal Tokens As List(Of Token), ByVal ParentFile As SourceFile) As Node

        'Empty
        If Not Tokens.Count > 0 Then
            Return Nothing
        End If

        'Reload informations
        Me.ParentFile = ParentFile
        Me.Tokens = Tokens
        TokenIndex = -1
        advance()
        Dim ReturnValue As Node = Nothing

        'Take value
        If CurrentToken.Type = TokenType.CODE_COLON Then

            'Type
            advance()
            ReturnValue = GetTypeNode()

        Else

            'Value
            ReturnValue = GetTopValue()

            'Error
            If TypeOf ReturnValue Is AddSourceDirectlyNode Then
                ThrowCoordinatesSyntaxLimException("ASTPV02", "An ""AddSourceDirectly"" node cannot be located here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

        End If

        'Error
        If TokenIndex < Tokens.Count - 1 Then
            ThrowCoordinatesSyntaxLimException("ASTPV01", "Nothing else was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If

        'Return
        Return ReturnValue

    End Function

    '==============================
    '========= PARSE FILE =========
    '==============================
    Public Sub ParseFile(ByVal Tokens As List(Of Token), ByVal ParentFile As SourceFile)

        'Empty
        If Not Tokens.Count > 0 Then
            Exit Sub
        End If

        'Reload informations
        Me.ParentFile = ParentFile
        Me.Tokens = Tokens
        TokenIndex = -1
        advance()

        While TokenIndex < Tokens.Count - 1

            Dim Result As Node = GetClass()
            Result.ParentNode = ParentFile

            If TypeOf Result Is ClassNode Then
                ParentFile.Classes.Add(Result)
            ElseIf TypeOf Result Is FunctionNode Then
                ParentFile.Functions.Add(Result)
            ElseIf TypeOf Result Is DeclareVariableNode Then
                ParentFile.DeclareVariables.Add(Result)
            ElseIf TypeOf Result Is AddSourceDirectlyStatementNode Then
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
            ThrowCoordinatesSyntaxLimException("ASTGT01", "A type name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim ResultNode As New TypeNode(CurrentToken.PositionStartX, CurrentToken.PositionStartY, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value)
        advance()

        'Arguments
        If CurrentToken.Type = TokenType.OP_LESSTHAN Then

            'Get Arguments
            advance()
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

        End If

        'Specially for function types
        'They are the only ones that can have two argument lists, the second is used to represent the type returned by the function.
        '   Like this: fun<int, int><float>
        '   This represents the function : func ~(~:int, ~:int):float
        'The type of the second list is insert at the beginning of the first list. If this does not exist then "Nothing" will be inserted.
        '   Like this : fun<float, int, int>
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
    Private Function GetFactor() As ValueNode

        Dim tok As Token = CurrentToken

        If tok.Type = TokenType.CT_INTEGER Or tok.Type = TokenType.CT_FLOAT Then
            advance()
            Return New NumericValueNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, tok)

        ElseIf tok.Type = TokenType.CT_STRING Then
            advance()
            Return New StringNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, tok.Value)

        ElseIf tok.Type = TokenType.CT_TRUE Then
            advance()
            Return New BooleanNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, True)

        ElseIf tok.Type = TokenType.CT_FALSE Then
            advance()
            Return New BooleanNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, False)

        ElseIf tok.Type = TokenType.CODE_TERM Then
            advance()
            Return New VariableNode(tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX, tok.Value)

        ElseIf tok.Type = TokenType.CODE_DOLLAR Then
            advance()
            If Not CurrentToken.Type = TokenType.CT_STRING Then
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

    '=========================
    '========= CHILD =========
    '=========================
    Private Function GetChild() As ValueNode

        Dim Value As ValueNode = GetFactor()

        While CurrentToken.Type = TokenType.CODE_POINT

            'Error
            advance()
            If Not CurrentToken.Type = TokenType.CODE_TERM Then
                ThrowCoordinatesSyntaxLimException("ASTGC01", "A property name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

            'Create node
            Value = New ChildNode(Value.PositionStartY, Value.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, Value, CurrentToken.Value)
            advance()

        End While

        Return Value

    End Function

    '=================================
    '========= FUNCTION CALL =========
    '=================================
    Private Function GetFunctionCall() As ValueNode

        Dim Value As ValueNode = GetChild()

        While CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS

            'Create node
            Dim NewValue As New FunctionCallNode(Value.PositionStartY, Value.PositionStartX, 0, 0, Value)

            'Arguments
            advance()
            If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then

                'Get all arguments
                While True

                    'Get the node
                    Dim Argument As ValueNode = GetTopValue()
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
    Private Function GetNew() As ValueNode
        Return GetFunctionCall()
    End Function

    '====================================
    '========= BRACKET SELECTOR =========
    '====================================
    Private Function GetBracketSelector() As ValueNode
        Return GetNew()
    End Function

    '===================================
    '========= UNARY OPERATION =========
    '===================================
    Private Function GetUnaryOperation() As ValueNode

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
    Private Function GetTerm() As ValueNode
        Return GetUnaryOperation()
    End Function

    '========================
    '========= EXPR =========
    '========================
    Private Function GetExpr() As ValueNode
        Return GetTerm()
    End Function

    '==============================
    '========= COMPARISON =========
    '==============================
    Private Function GetComparison() As ValueNode
        Return GetExpr()
    End Function

    '=====================================
    '========= BOOLEAN OPERATION =========
    '=====================================
    Private Function GetBooleanOperation() As ValueNode
        Return GetComparison()
    End Function

    '==================================
    '=========  GET TOP VALUE =========
    '==================================
    Private Function GetTopValue() As ValueNode
        Return GetBooleanOperation()
    End Function

    '=============================
    '=========  GET LINE =========
    '=============================
    Private Function GetLine() As StatementNode

        'Tok
        Dim tok As Token = CurrentToken

        'Declare variable
        If CurrentToken.Type = TokenType.KW_LET Then

            'Export
            advance()
            Dim Export As Boolean = False
            If CurrentToken.Type = TokenType.KW_EXPORT Then
                Export = True
                advance()
            End If

            'Get name
            Dim VariableName As String
            Dim CustomName As Boolean
            If CurrentToken.Type = TokenType.CODE_TERM Then

                'Get name
                CustomName = False
                VariableName = CurrentToken.Value
                advance()


            ElseIf CurrentToken.Type = TokenType.CODE_DOLLAR Then

                'Custom name
                advance()
                If Not CurrentToken.Type = TokenType.CT_STRING Then
                    ThrowCoordinatesSyntaxLimException("ASTGL01", "A string was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                If CurrentToken.Value = "" Then
                    ThrowCoordinatesSyntaxLimException("ASTGL02", "A varaible name must be specified here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                CustomName = True
                VariableName = CurrentToken.Value
                advance()

            Else

                'Error
                ThrowCoordinatesSyntaxLimException("ASTGL03", "The variable name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                VariableName = ""

            End If

            'Type
            Dim ExplicitType As TypeNode = Nothing
            If CurrentToken.Type = TokenType.CODE_COLON Then

                'Get type
                advance()
                ExplicitType = GetTypeNode()

            End If

            'Value
            Dim ExplicitValue As ValueNode = Nothing
            If CurrentToken.Type = TokenType.OP_EQUAL Then

                'Get value
                advance()
                ExplicitValue = GetTopValue()

            End If

            'No type and no value
            If ExplicitType Is Nothing And ExplicitValue Is Nothing Then
                ThrowCoordinatesSyntaxLimException("ASTGL04", "To declare a variable, you must at least explicitly declare its type or a default value.", ParentFile, tok.PositionStartY, tok.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

            'Create node
            Return New DeclareVariableNode(tok.PositionStartY, tok.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, VariableName, CustomName, ExplicitType, ExplicitValue, Export)

        End If

        'Deep call
        Dim Value As Node = GetTopValue()

        'Function call
        If TypeOf Value Is FunctionCallNode Then
            Return New FunctionCallStatementNode(Value)
        End If

        'Add source directly
        If TypeOf Value Is AddSourceDirectlyNode Then
            Return New AddSourceDirectlyStatementNode(Value)
        End If

        'Else
        ThrowCoordinatesSyntaxLimException("ASTGL00", "A line of code was expected here.", ParentFile, tok.PositionStartY, tok.PositionStartX, tok.PositionEndY, tok.PositionEndX)
        Return Nothing

    End Function

    '=================================
    '=========  GET RELATION =========
    '=================================
    Private Function GetRelation() As Node

        'Save the state
        Dim SavedIndex As Integer = TokenIndex

        'Indentation
        If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
            ThrowCoordinatesSyntaxLimException("ASTGR01", "The start of a new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim IndentationToken As Token = CurrentToken
        Dim CurrentScopeIndentation As Integer = DirectCast(IndentationToken.Value, Integer)
        advance()

        'Is a relation
        If Not CurrentToken.Type = TokenType.KW_RELATION Then
            recede(SavedIndex + 1)
            Return GetLine()
        End If
        advance()

        'Check indentation level
        If CurrentScopeIndentation > 1 Then
            ThrowCoordinatesSyntaxLimException("ASTGR02", "A relation is necessarily part of a class, so the definition line is necessarily indented by 1. However, the indentation level is " & CurrentScopeIndentation.ToString() & " here.", ParentFile, IndentationToken.PositionStartY, IndentationToken.PositionStartX, IndentationToken.PositionEndY, IndentationToken.PositionEndX)
        End If

        'Function Name
        If Not CurrentToken.Type = TokenType.CODE_TERM Then
            ThrowCoordinatesSyntaxLimException("ASTGR03", "The function name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim ResultNode As New RelationNode(Tokens(TokenIndex).PositionStartY, Tokens(TokenIndex).PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken)
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
                        ThrowCoordinatesSyntaxLimException("ASTGR04", "Argument name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim ArgumentNameToken As Token = CurrentToken
                    advance()

                    'Get argument type
                    Dim ArgumentType As TypeNode = Nothing
                    If CurrentToken.Type = TokenType.CODE_COLON Then
                        advance()
                        ArgumentType = GetTypeNode()
                    End If

                    'Optionnel
                    If CurrentToken.Type = TokenType.OP_EQUAL Then
                        ThrowCoordinatesSyntaxLimException("ASTGR05", "The arguments of a relation cannot be optional.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If

                    'Add argument
                    ResultNode.RelationArguments.Add(New FunctionArgumentNode(ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentType.PositionEndY, ArgumentType.PositionEndX, ArgumentNameToken.Value, ArgumentType, Nothing))

                    'Continue
                    If CurrentToken.Type = TokenType.OP_COMMA Then
                        advance()
                        Continue While
                    ElseIf CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                        Exit While
                    Else
                        ThrowCoordinatesSyntaxLimException("ASTGR06", "A comma or closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
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
                ThrowCoordinatesSyntaxLimException("ASTGR07", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
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
        Dim ResultNode As FunctionNode
        If CurrentToken.Type = TokenType.CODE_DOLLAR Then

            advance()
            If Not CurrentToken.Type = TokenType.CT_STRING Then
                ThrowCoordinatesSyntaxLimException("ASTGF09", "A string was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            If CurrentToken.Value = "" Then
                ThrowCoordinatesSyntaxLimException("ASTGF11", "A function name and any parameters must be specified here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            ResultNode = New FunctionNode(Tokens(TokenIndex).PositionStartY, Tokens(TokenIndex).PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value)
            advance()
            If CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS Then
                ThrowCoordinatesSyntaxLimException("ASTGF10", "No parameters can be specified for such a function.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

        Else

            If Not CurrentToken.Type = TokenType.CODE_TERM Then
                ThrowCoordinatesSyntaxLimException("ASTGF03", "The function name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            ResultNode = New FunctionNode(Tokens(TokenIndex).PositionStartY, Tokens(TokenIndex).PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value, export)
            advance()

        End If

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
                    Dim ResultArgument As New FunctionArgumentNode(ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentPositionEndY, ArgumentPositionEndX, ArgumentNameToken.Value, ArgumentType, ArgumentValue)
                    ResultArgument.ParentNode = ResultNode
                    ResultNode.FunctionArguments.Add(ResultArgument)

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
            ThrowCoordinatesSyntaxLimException("ASTGC08", "The class name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If
        Dim ResultNode As New ClassNode(Tokens(TokenIndex).PositionStartY, Tokens(TokenIndex).PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value, export, primary)
        If {"int", "float", "str", "bool", "any", "fun"}.Contains(CurrentToken.Value) Then
            If Not ParentFile.filepath = executableDirectory & "/libs/std.lim" Then
                ThrowCoordinatesSyntaxLimException("ASTGC03", "The class """ & CurrentToken.Value & """ is already defined.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Select Case CurrentToken.Value
                Case "int"
                    Compiler.STD_int = New Type(ResultNode, New List(Of Type), False)
                Case "float"
                    Compiler.STD_float = New Type(ResultNode, New List(Of Type), False)
                Case "str"
                    Compiler.STD_str = New Type(ResultNode, New List(Of Type), False)
                Case "bool"
                    Compiler.STD_bool = New Type(ResultNode, New List(Of Type), False)
                Case "any"
                    Compiler.STD_any = New Type(ResultNode, New List(Of Type), False)
                Case "fun"
                    Compiler.STD_funClass = ResultNode
            End Select
        End If
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
            ElseIf TypeOf LineResult Is AddSourceDirectlyStatementNode Then
                ResultNode.AddSourcesDirectly.Add(LineResult)
            Else
                ThrowNodeSyntaxException("ASTGC07", "This was not expected here.", LineResult, "Check line indentation.")
            End If


        End While

        'Return
        Return ResultNode

    End Function

End Class
