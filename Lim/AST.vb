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
            ElseIf TypeOf Result Is ExtendNode Then
                ParentFile.Extends.Add(Result)
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
        Dim ResultNode As New TypeNode(CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value)
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
                SecondArgument.ParentNode = ResultNode
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

        ElseIf tok.Type = TokenType.OP_LEFT_BRACKET Then
            advance()
            If CurrentToken.Type = TokenType.OP_RIGHT_BRACKET Then
                ThrowCoordinatesSyntaxLimException("ASTGF06", "An empty list cannot be declared this way because its type cannot be inferred.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Dim Values As New List(Of ValueNode)
            While True
                Values.Add(GetTopValue())
                If CurrentToken.Type = TokenType.OP_RIGHT_BRACKET Then
                    Exit While
                ElseIf CurrentToken.Type = TokenType.OP_COMMA Then
                    advance()
                Else
                    ThrowCoordinatesSyntaxLimException("ASTGF05", "A comma or a closing bracket was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
            End While
            Dim Result As New ListNode(tok.PositionStartY, tok.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, Values)
            advance()
            Return Result

        ElseIf tok.Type = TokenType.CODE_LINEINDENTATION Then
            ThrowCoordinatesSyntaxLimException("ASTGF04", "The previous line is not complete.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)

        End If

        ThrowCoordinatesSyntaxLimException("ASTGFA01", "A value was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
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

            'Child of
            While CurrentToken.Type = TokenType.CODE_POINT

                'Error
                advance()
                If Not CurrentToken.Type = TokenType.CODE_TERM Then
                    ThrowCoordinatesSyntaxLimException("ASTGFC02", "A property name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If

                'Create node
                Value = New ChildNode(Value.PositionStartY, Value.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, Value, CurrentToken.Value)
                advance()

            End While


        End While

        Return Value

    End Function

    '============================
    '========= NEW NODE =========
    '============================
    Private Function GetNew() As ValueNode

        If CurrentToken.Type = TokenType.KW_NEW Then

            'Get type
            Dim tok As Token = CurrentToken
            advance()
            Dim TargetType As TypeNode = GetTypeNode()
            Dim EndPosY As Integer = TargetType.PositionEndY
            Dim EndPosX As Integer = TargetType.PositionEndX

            'Get arguments
            Dim PassedArguments As New List(Of ValueNode)
            If CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS Then
                advance()
                If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                    While True

                        'Get value
                        PassedArguments.Add(GetTopValue())

                        'Continue loop
                        If CurrentToken.Type = TokenType.OP_COMMA Then
                            advance()
                            Continue While
                        ElseIf CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                            Exit While
                        Else
                            ThrowCoordinatesSyntaxLimException("ASTGN01", "A comma or closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                        End If

                    End While
                End If
                EndPosY = CurrentToken.PositionEndY
                EndPosX = CurrentToken.PositionEndY
                advance()
            End If

            'Return node
            Return New NewNode(tok.PositionStartY, tok.PositionStartX, EndPosY, EndPosX, TargetType, PassedArguments)

        End If

        Return GetFunctionCall()

    End Function

    '====================================
    '========= BRACKET SELECTOR =========
    '====================================
    Private Function GetBracketSelector() As ValueNode

        Dim Left As ValueNode = GetNew()
        While CurrentToken.Type = TokenType.OP_LEFT_BRACKET

            advance()
            Dim Index As ValueNode = GetTopValue()
            If Not CurrentToken.Type = TokenType.OP_RIGHT_BRACKET Then
                ThrowCoordinatesSyntaxLimException("ASTGBS01", "A closing bracket was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Left = New BracketSelectorNode(Left.PositionStartY, Left.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, Left, Index)
            advance()

        End While
        Return Left

    End Function

    '===================================
    '========= UNARY OPERATION =========
    '===================================
    Private Function GetUnaryOperation() As ValueNode

        If CurrentToken.Type = TokenType.OP_MINUS Then
            Dim Tok As Token = CurrentToken
            advance()
            Dim Target As Node = GetBracketSelector()
            Return New UnaryOpNode(Tok.PositionStartY, Tok.PositionStartX, Target.PositionEndY, Target.PositionEndX, Tok, Target)
        Else
            Return GetBracketSelector()
        End If

    End Function

    '========================
    '========= TERM =========
    '========================
    Private Function GetTerm() As ValueNode

        Dim Left As ValueNode = GetUnaryOperation()
        While {TokenType.OP_MULTIPLICATION, TokenType.OP_DIVISION}.Contains(CurrentToken.Type)

            Dim Op As Token = CurrentToken
            advance()
            Dim Right As ValueNode = GetUnaryOperation()
            Left = New BinaryOperationNode(Left.PositionStartY, Left.PositionStartX, Right.PositionEndY, Right.PositionEndX, Left, Right, Op)

        End While

        Return Left

    End Function

    '========================
    '========= EXPR =========
    '========================
    Private Function GetExpr() As ValueNode

        Dim Left As ValueNode = GetTerm()
        While {TokenType.OP_PLUS, TokenType.OP_MINUS}.Contains(CurrentToken.Type)

            Dim Op As Token = CurrentToken
            advance()
            Dim Right As ValueNode = GetTerm()
            Left = New BinaryOperationNode(Left.PositionStartY, Left.PositionStartX, Right.PositionEndY, Right.PositionEndX, Left, Right, Op)

        End While

        Return Left

    End Function

    '==============================
    '========= COMPARISON =========
    '==============================
    Private Function GetComparison() As ValueNode

        Dim Left As ValueNode = GetExpr()
        While {TokenType.OP_EQUAL, TokenType.OP_LESSTHAN, TokenType.OP_LESSTHANEQUAL, TokenType.OP_MORETHAN, TokenType.OP_MORETHANEQUAL}.Contains(CurrentToken.Type)

            Dim Op As Token = CurrentToken
            advance()
            Dim Right As ValueNode = GetExpr()
            Left = New ComparisonNode(Left.PositionStartY, Left.PositionStartX, Right.PositionEndY, Right.PositionEndX, Left, Right, Op)

        End While

        Return Left

    End Function

    '=====================================
    '========= BOOLEAN OPERATION =========
    '=====================================
    Private Function GetBooleanOperation() As ValueNode

        Dim Left As ValueNode = GetComparison()
        While {TokenType.OP_AND, TokenType.OP_OR, TokenType.OP_NOT}.Contains(CurrentToken.Type)

            Dim Op As Token = CurrentToken
            advance()
            Dim Right As ValueNode = GetComparison()
            Left = New BooleanOperationNode(Left.PositionStartY, Left.PositionStartX, Right.PositionEndY, Right.PositionEndX, Left, Right, Op)

        End While

        Return Left

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
    Private Function GetLine(ByVal CurrentLineIndentation As Integer) As StatementNode

        'Tok
        Dim tok As Token = CurrentToken

        'Platform
        While CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS

            'Save
            Dim recedeIndex As Integer = TokenIndex

            'Get value
            advance()
            If CurrentToken.Type = TokenType.CODE_DOLLAR Then

                advance()
                If Not CurrentToken.Type = TokenType.CODE_TERM Then
                    ThrowCoordinatesSyntaxLimException("ASTGL15", "A platform name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                Dim TargetedPlatform As Platform = Nothing
                Select Case DirectCast(CurrentToken.Value, String).ToLower()

                    Case "linux"
                        TargetedPlatform = Platform.Linux

                    Case "windows"
                        TargetedPlatform = Platform.Win

                    Case "noconsole"
                        TargetedPlatform = Platform.NoConsole

                    Case "console"
                        TargetedPlatform = Platform.Console

                    Case Else
                        ThrowCoordinatesSyntaxLimException("ASTGL16", "Unrecognized platform name.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)

                End Select
                advance()
                If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                    ThrowCoordinatesSyntaxLimException("ASTGL17", "A closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                advance()
                Dim Value As StatementNode = GetLine(CurrentLineIndentation)
                Return New PlatformDependentStatement(tok.PositionStartY, tok.PositionStartX, TargetedPlatform, Value)

            Else

                TokenIndex = recedeIndex

            End If

        End While

        'Declare variable
        If CurrentToken.Type = TokenType.KW_LET Then

            'Export
            advance()
            Dim Export As Boolean = False
            If CurrentToken.Type = TokenType.KW_EXPORT Then
                Export = True
                advance()
            End If

            'Position
            Dim EndPosY As Integer = CurrentToken.PositionEndY
            Dim EndPosX As Integer = CurrentToken.PositionEndX

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
                EndPosY = ExplicitType.PositionEndY
                EndPosX = ExplicitType.PositionEndX

            End If

            'Value
            Dim ExplicitValue As ValueNode = Nothing
            If CurrentToken.Type = TokenType.OP_EQUAL Then

                'Get value
                advance()
                ExplicitValue = GetTopValue()
                EndPosY = ExplicitValue.PositionEndY
                EndPosX = ExplicitValue.PositionEndX

            End If

            'No type and no value
            If ExplicitType Is Nothing And ExplicitValue Is Nothing Then
                ThrowCoordinatesSyntaxLimException("ASTGL04", "To declare a variable, you must at least explicitly declare its type or a default value.", ParentFile, tok.PositionStartY, tok.PositionStartX, EndPosY, EndPosX)
            End If

            'Create node
            Return New DeclareVariableNode(tok.PositionStartY, tok.PositionStartX, EndPosY, EndPosX, VariableName, CustomName, ExplicitType, ExplicitValue, Export)

        End If

        'Return
        If CurrentToken.Type = TokenType.KW_RETURN Then

            'Save position
            Dim StartY As Integer = CurrentToken.PositionStartY
            Dim StartX As Integer = CurrentToken.PositionStartX
            Dim EndY As Integer = CurrentToken.PositionEndY
            Dim EndX As Integer = CurrentToken.PositionEndX
            advance()

            'Get value
            Dim Value As ValueNode = Nothing
            If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                Value = GetTopValue()
                EndY = Value.PositionEndY
                EndX = Value.PositionEndX
            End If

            'Create node
            Return New ReturnNode(StartY, StartX, EndY, EndX, Value)

        End If

        'For & Foreach
        If CurrentToken.Type = TokenType.KW_FOR Then

            'Save position
            Dim StartY As Integer = CurrentToken.PositionStartY
            Dim StartX As Integer = CurrentToken.PositionStartX
            advance()

            'Get variable
            If Not CurrentToken.Type = TokenType.CODE_TERM Then
                ThrowCoordinatesSyntaxLimException("ASTGL05", "The name of the variable representing the iterator was expected here.", ParentFile, tok.PositionStartY, tok.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Dim VariableName As String = CurrentToken.Value
            advance()

            'Type
            Dim ExplicitType As TypeNode = Nothing
            If CurrentToken.Type = TokenType.CODE_COLON Then
                advance()
                ExplicitType = GetTypeNode()
            End If

            'For each
            If CurrentToken.Type = TokenType.OP_IN Then

                'Get target
                advance()
                Dim Target As ValueNode = GetTopValue()

                'Create node
                Dim ResultNode As New ForeachNode(StartY, StartX, Target.PositionEndY, Target.PositionEndX, VariableName, ExplicitType, Target)

                'Content
                While True

                    'NewLine
                    If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                        ThrowCoordinatesSyntaxLimException("ASTGL06", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                    If LineIndentation <= CurrentLineIndentation Then
                        Exit While
                    End If

                    'Get content
                    advance()
                    Dim LineResult As Node = GetLine(LineIndentation)
                    LineResult.ParentNode = ResultNode
                    ResultNode.Codes.Add(LineResult)

                End While

                'Return
                Return ResultNode

            ElseIf CurrentToken.Type = TokenType.KW_FROM Then

                'Get from
                advance()
                Dim FromValue As ValueNode = GetTopValue()

                'Get to
                If Not CurrentToken.Type = TokenType.KW_TO Then
                    ThrowCoordinatesSyntaxLimException("ASTGL08", "The keyword ""to"" was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                advance()
                Dim ToValue As ValueNode = GetTopValue()

                'Create node
                Dim ResultNode As New ForNode(StartY, StartX, ToValue.PositionEndY, ToValue.PositionEndX, VariableName, ExplicitType, FromValue, ToValue)

                'Content
                While True

                    'NewLine
                    If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                        ThrowCoordinatesSyntaxLimException("ASTGL07", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                    If LineIndentation <= CurrentLineIndentation Then
                        Exit While
                    End If

                    'Get content
                    advance()
                    Dim LineResult As Node = GetLine(LineIndentation)
                    LineResult.ParentNode = ResultNode
                    ResultNode.Codes.Add(LineResult)

                End While

                'Return
                Return ResultNode


            ElseIf CurrentToken.Type = TokenType.KW_TO Then

                'Get to
                advance()
                Dim ToValue As ValueNode = GetTopValue()

                'Create node
                Dim ResultNode As New ForNode(StartY, StartX, ToValue.PositionEndY, ToValue.PositionEndX, VariableName, ExplicitType, Nothing, ToValue)

                'Content
                While True

                    'NewLine
                    If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                        ThrowCoordinatesSyntaxLimException("ASTGL09", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                    If LineIndentation <= CurrentLineIndentation Then
                        Exit While
                    End If

                    'Get content
                    advance()
                    Dim LineResult As Node = GetLine(LineIndentation)
                    LineResult.ParentNode = ResultNode
                    ResultNode.Codes.Add(LineResult)

                End While

                'Return
                Return ResultNode

            Else
                ThrowCoordinatesSyntaxLimException("ASTGL06", "The keywords ""from"" or ""in"" were expected here.", ParentFile, tok.PositionStartY, tok.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

        End If

        'While
        If CurrentToken.Type = TokenType.KW_WHILE Then

            'Get condition
            advance()
            Dim Condition As ValueNode = GetTopValue()

            'Create node
            Dim ResultNode As New WhileNode(tok.PositionStartY, tok.PositionStartX, Condition.PositionEndY, Condition.PositionEndX, Condition)

            'Content
            While True

                'NewLine
                If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                    ThrowCoordinatesSyntaxLimException("ASTGL10", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                If LineIndentation <= CurrentLineIndentation Then
                    Exit While
                End If

                'Get content
                advance()
                Dim LineResult As Node = GetLine(LineIndentation)
                LineResult.ParentNode = ResultNode
                ResultNode.Codes.Add(LineResult)

            End While

            'Return
            Return ResultNode

        End If

        'If statement
        If CurrentToken.Type = TokenType.KW_IF Then

            'Create node
            Dim ResultNode As New IfNode(tok.PositionStartY, tok.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)

            'Get condition
            advance()
            ResultNode.MainCondition = GetTopValue()
            ResultNode.MainCondition.ParentNode = ResultNode

            'Content
            While True

                'NewLine
                If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                    ThrowCoordinatesSyntaxLimException("ASTGL11", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                End If
                Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                If LineIndentation <= CurrentLineIndentation Then
                    Exit While
                End If

                'Get content
                advance()
                Dim LineResult As Node = GetLine(LineIndentation)
                LineResult.ParentNode = ResultNode
                ResultNode.MainCodes.Add(LineResult)

            End While

            'Else if
            Dim elseif_statement As New List(Of Tuple(Of ValueNode, List(Of StatementNode)))
            While CurrentToken.Type = TokenType.CODE_LINEINDENTATION

                'Advance
                If TokenIndex = Tokens.Count - 1 Then
                    Exit While
                End If
                advance()

                'Keyword
                If Not CurrentToken.Type = TokenType.KW_ELSEIF Then
                    recede(TokenIndex - 1)
                    Exit While
                End If
                advance()

                'Get condition
                Dim elseif_condition As ValueNode = GetTopValue()
                elseif_condition.ParentNode = ResultNode

                'Get if lines
                Dim elseif_lines As New List(Of StatementNode)

                'Content
                While True

                    'NewLine
                    If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                        ThrowCoordinatesSyntaxLimException("ASTGL12", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                    If LineIndentation <= CurrentLineIndentation Then
                        Exit While
                    End If

                    'Get content
                    advance()
                    Dim LineResult As Node = GetLine(LineIndentation)
                    LineResult.ParentNode = ResultNode
                    ResultNode.MainCodes.Add(LineResult)

                End While

                'Add
                elseif_statement.Add((elseif_condition, elseif_lines).ToTuple())

            End While
            ResultNode.ElseIfs = elseif_statement

            'Else
            Dim recedeIndex As Integer = TokenIndex
            If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                ThrowCoordinatesSyntaxLimException("ASTGL13", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            If TokenIndex = Tokens.Count - 1 Then
                Return ResultNode
            End If
            advance()

            'Else
            Dim else_statement As New List(Of StatementNode)
            If CurrentToken.Type = TokenType.KW_ELSE Then

                'Advance
                advance()

                'Content
                While True

                    'NewLine
                    If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                        ThrowCoordinatesSyntaxLimException("ASTGL14", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If
                    Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
                    If LineIndentation <= CurrentLineIndentation Then
                        Exit While
                    End If

                    'Get content
                    advance()
                    Dim LineResult As Node = GetLine(LineIndentation)
                    LineResult.ParentNode = ResultNode
                    ResultNode.ElseCodes.Add(LineResult)

                End While

            Else

                recede(recedeIndex)

            End If

            'Return
            Return ResultNode

        End If

        'Deep call
        Dim LineValueNode As Node = GetTopValue()

        'SetVariable
        If TypeOf LineValueNode Is ComparisonNode Then
            Dim CastedComparison As ComparisonNode = DirectCast(LineValueNode, ComparisonNode)
            If CastedComparison.Op = RelationOperator.EQUAL And (TypeOf CastedComparison.Left Is VariableNode Or TypeOf CastedComparison.Left Is ChildNode Or TypeOf CastedComparison.Left Is BracketSelectorNode) Then
                Return New SetVariableNode(CastedComparison)
            End If
        End If

        'Function call
        If TypeOf LineValueNode Is FunctionCallNode Then
            Return New FunctionCallStatementNode(LineValueNode)
        End If

        'Add source directly
        If TypeOf LineValueNode Is AddSourceDirectlyNode Then
            Return New AddSourceDirectlyStatementNode(LineValueNode)
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

        'Extend
        Dim ExtendTarget As String = ""
        If CurrentToken.Type = TokenType.KW_EXTEND Then
            advance()
            If Not CurrentToken.Type = TokenType.CODE_TERM Then
                ThrowCoordinatesSyntaxLimException("ASTGR23", "The name of a class was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            ExtendTarget = CurrentToken.Value
            advance()
        End If

        'Is a relation
        If Not CurrentToken.Type = TokenType.KW_RELATION Then
            recede(SavedIndex + 1)
            Return GetLine(CurrentScopeIndentation)
        End If
        Dim PositionStartY As Integer = CurrentToken.PositionStartY
        Dim PositionStartX As Integer = CurrentToken.PositionStartX
        advance()

        'Check indentation level
        If CurrentScopeIndentation > 1 Then
            ThrowCoordinatesSyntaxLimException("ASTGR02", "A relation is necessarily part of a class, so the definition line is necessarily indented by 1. However, the indentation level is " & CurrentScopeIndentation.ToString() & " here.", ParentFile, IndentationToken.PositionStartY, IndentationToken.PositionStartX, IndentationToken.PositionEndY, IndentationToken.PositionEndX)
        End If

        'Relation operator
        Dim RelationOperator As RelationOperator = Nothing
        If CurrentToken.Type = TokenType.OP_PLUS Then
            RelationOperator = RelationOperator.PLUS
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_MINUS Then
            RelationOperator = RelationOperator.MINUS
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_MULTIPLICATION Then
            RelationOperator = RelationOperator.MULTIPLICATION
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_DIVISION Then
            RelationOperator = RelationOperator.DIVISION
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_EQUAL Then
            RelationOperator = RelationOperator.EQUAL
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_LESSTHAN Then
            RelationOperator = RelationOperator.LESSTHAN
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_LESSTHANEQUAL Then
            RelationOperator = RelationOperator.LESSTHANEQUAL
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_MORETHAN Then
            RelationOperator = RelationOperator.MORETHAN
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_MORETHANEQUAL Then
            RelationOperator = RelationOperator.MORETHANEQUAL
            advance()
        ElseIf CurrentToken.Type = TokenType.OP_LEFT_BRACKET Then
            advance()
            If Not CurrentToken.Type = TokenType.OP_RIGHT_BRACKET Then
                ThrowCoordinatesSyntaxLimException("ASTGR03", "The ""]"" character was expected here", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            RelationOperator = RelationOperator.INDEX
            advance()
        ElseIf CurrentToken.Type = TokenType.CODE_TERM Then
            If CurrentToken.Value = "for_from" Then
                RelationOperator = RelationOperator.FOR_FROM
                advance()
            ElseIf CurrentToken.Value = "for_to" Then
                RelationOperator = RelationOperator.FOR_TO
                advance()
            ElseIf CurrentToken.Value = "for_iteration" Then
                RelationOperator = RelationOperator.FOR_ITERATION
                advance()
            Else
                ThrowCoordinatesSyntaxLimException("ASTGR19", "Unknown relation name.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
        Else
            ThrowCoordinatesSyntaxLimException("ASTGR04", "The relation operator was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
        End If

        'Arguments
        Dim Arguments As New List(Of FunctionArgumentNode)
        If CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS Then

            'No arguments
            advance()
            If Not CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then

                While True

                    'Get argument name
                    If Not CurrentToken.Type = TokenType.CODE_TERM Then
                        ThrowCoordinatesSyntaxLimException("ASTGR05", "Argument name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
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
                        ThrowCoordinatesSyntaxLimException("ASTGR06", "The arguments of a relation cannot be optional.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If

                    'Add argument
                    Arguments.Add(New FunctionArgumentNode(ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentType.PositionEndY, ArgumentType.PositionEndX, ArgumentNameToken.Value, ArgumentType, Nothing))

                    'Continue
                    If CurrentToken.Type = TokenType.OP_COMMA Then
                        advance()
                        Continue While
                    ElseIf CurrentToken.Type = TokenType.OP_RIGHT_PARENTHESIS Then
                        Exit While
                    Else
                        ThrowCoordinatesSyntaxLimException("ASTGR07", "A comma or closing parenthesis was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
                    End If

                End While

            End If
            advance()

        End If

        'Create node
        Dim ResultNode As New RelationNode(PositionStartY, PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, RelationOperator, Arguments)

        'Verify operator & arguments & relation type
        Select Case RelationOperator

            Case RelationOperator.PLUS
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR08", "The ""+"" operator must take two arguments because its relation is of type: (a + b)", ResultNode)
                End If

            Case RelationOperator.MINUS
                If Arguments.Count = 1 Then
                    ResultNode.RelationOperator = RelationOperator.UNARY_MINUS
                    RelationOperator = RelationOperator.UNARY_MINUS
                    Exit Select
                End If
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR09", "The ""-"" operator must take one or two arguments because its relations are of the type: (a + b) or (-a)", ResultNode)
                End If

            Case RelationOperator.MULTIPLICATION
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR10", "The ""*"" operator must take two arguments because its relation is of type: (a * b)", ResultNode)
                End If

            Case RelationOperator.DIVISION
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR11", "The ""/"" operator must take two arguments because its relation is of type: (a / b)", ResultNode)
                End If

            Case RelationOperator.EQUAL
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR12", "The ""="" operator must take two arguments because its relation is of type: (a = b)", ResultNode)
                End If

            Case RelationOperator.LESSTHAN
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR13", "The ""<"" operator must take two arguments because its relation is of type: (a < b)", ResultNode)
                End If

            Case RelationOperator.LESSTHANEQUAL
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR14", "The ""<="" operator must take two arguments because its relation is of type: (a <= b)", ResultNode)
                End If

            Case RelationOperator.MORETHAN
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR15", "The "">"" operator must take two arguments because its relation is of type: (a > b)", ResultNode)
                End If

            Case RelationOperator.MORETHANEQUAL
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR16", "The "">="" operator must take two arguments because its relation is of type: (a >= b)", ResultNode)
                End If

            Case RelationOperator.INDEX
                If Arguments.Count = 3 Then
                    ResultNode.RelationOperator = RelationOperator.INDEX_SET
                    RelationOperator = RelationOperator.INDEX_SET
                    Exit Select
                End If
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR17", "The ""[]"" operator must take two or three arguments because its relations are of type: ( a[b] ) or ( a[b] = c )", ResultNode)
                End If

            Case RelationOperator.FOR_FROM
                If Not Arguments.Count = 1 Then
                    ThrowNodeSyntaxException("ASTGR20", "The ""for_from"" relation must take one arguments.", ResultNode)
                End If

            Case RelationOperator.FOR_TO
                If Not Arguments.Count = 1 Then
                    ThrowNodeSyntaxException("ASTGR21", "The ""for_to"" relation must take one arguments.", ResultNode)
                End If

            Case RelationOperator.FOR_ITERATION
                If Not Arguments.Count = 2 Then
                    ThrowNodeSyntaxException("ASTGR22", "The ""for_iteration"" relation must take one arguments.", ResultNode)
                End If

            Case Else
                Throw New NotImplementedException()

        End Select

        'Return type
        If CurrentToken.Type = TokenType.CODE_COLON Then
            advance()
            Dim ReturnTypeNode As TypeNode = GetTypeNode()
            ReturnTypeNode.ParentNode = ResultNode
            ResultNode.ReturnTypeNode = ReturnTypeNode
        End If

        'Content
        While True

            'NewLine
            If Not CurrentToken.Type = TokenType.CODE_LINEINDENTATION Then
                ThrowCoordinatesSyntaxLimException("ASTGR18", "A new line was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Dim LineIndentation As Integer = DirectCast(CurrentToken.Value, Integer)
            If LineIndentation <= CurrentScopeIndentation Then
                Exit While
            End If

            'Get content
            advance()
            Dim LineResult As Node = GetLine(LineIndentation)
            LineResult.ParentNode = ResultNode
            ResultNode.Codes.Add(LineResult)
            ResultNode.PositionEndY = LineResult.PositionEndY
            ResultNode.PositionEndX = LineResult.PositionEndX

        End While

        'Return
        If ExtendTarget = "" Then
            Return ResultNode
        Else
            Return New ExtendNode(PositionStartY, PositionStartX, ResultNode.PositionEndY, ResultNode.PositionEndX, ExtendTarget, ResultNode)
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
        Dim PositionStartY As Integer = CurrentToken.PositionStartY
        Dim PositionStartX As Integer = CurrentToken.PositionStartX

        'Export
        Dim export As Boolean = False
        If CurrentToken.Type = TokenType.KW_EXPORT Then
            export = True
            advance()
        End If

        'Extend
        Dim ExtendTarget As String = ""
        If CurrentToken.Type = TokenType.KW_EXTEND Then
            If export Then
                ThrowCoordinatesSyntaxLimException("ASTGF09", "The ""extend"" block extends a class, so it already has an effect on all source files.", ParentFile, PositionStartY, PositionStartX, Tokens(TokenIndex - 1).PositionEndY, Tokens(TokenIndex - 1).PositionEndX, "Remove the ""export"" keyword")
            End If
            advance()
            If Not CurrentToken.Type = TokenType.CODE_TERM Then
                ThrowCoordinatesSyntaxLimException("ASTGF10", "The name of a class was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            ExtendTarget = CurrentToken.Value
            advance()
        End If

        'Is a function
        If Not CurrentToken.Type = TokenType.KW_FUNC Then
            recede(SavedIndex)
            Return GetRelation()
        End If
        advance()

        'Check indentation level
        If CurrentScopeIndentation > 1 Then
            ThrowCoordinatesSyntaxLimException("ASTGF02", "A function does not need an indentation while a method only needs an indentation. However, the indentation level is " & CurrentScopeIndentation.ToString() & " here.", ParentFile, IndentationToken.PositionStartY, IndentationToken.PositionStartX, IndentationToken.PositionEndY, IndentationToken.PositionEndX)
        End If

        'Function Name
        Dim FunctionName As String = Nothing
        Dim CustomHeader As String = Nothing
        If CurrentToken.Type = TokenType.CODE_DOLLAR Then

            advance()
            If Not CurrentToken.Type = TokenType.CT_STRING Then
                ThrowCoordinatesSyntaxLimException("ASTGF09", "A string was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            If export Then
                ThrowCoordinatesSyntaxLimException("ASTGF12", "A custom function is by definition accessible throughout the project. It is therefore useless to use the keyword ""export""", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            If CurrentToken.Value = "" Then
                ThrowCoordinatesSyntaxLimException("ASTGF11", "A function name and any parameters must be specified here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            CustomHeader = CurrentToken.Value
            advance()
            If CurrentToken.Type = TokenType.OP_LEFT_PARENTHESIS Then
                ThrowCoordinatesSyntaxLimException("ASTGF10", "No parameters can be specified for such a function.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If

        ElseIf CurrentToken.Type = TokenType.KW_NEW Then
            FunctionName = "new"
            advance()

        Else

            If Not CurrentToken.Type = TokenType.CODE_TERM Then
                ThrowCoordinatesSyntaxLimException("ASTGF03", "The function name was expected here.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            FunctionName = CurrentToken.Value
            advance()

        End If

        'Arguments
        Dim Arguments As New List(Of FunctionArgumentNode)
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
                    Arguments.Add(New FunctionArgumentNode(ArgumentNameToken.PositionStartY, ArgumentNameToken.PositionStartX, ArgumentPositionEndY, ArgumentPositionEndX, ArgumentNameToken.Value, ArgumentType, ArgumentValue))

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
        Dim ReturnType As TypeNode = Nothing
        If CurrentToken.Type = TokenType.CODE_COLON Then
            advance()
            ReturnType = GetTypeNode()
        End If

        'Create node
        Dim ResultNode As FunctionNode
        If CustomHeader = Nothing Then
            ResultNode = New FunctionNode(PositionStartY, PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, FunctionName, export, Arguments, ReturnType)
        Else
            ResultNode = New FunctionNode(PositionStartY, PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CustomHeader, ReturnType)
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
            Dim LineResult As Node = GetLine(LineIndentation)
            LineResult.ParentNode = ResultNode
            ResultNode.Codes.Add(LineResult)

        End While

        'Return
        If ExtendTarget = "" Then
            Return ResultNode
        Else
            Return New ExtendNode(PositionStartY, PositionStartX, ResultNode.PositionEndY, ResultNode.PositionEndX, ExtendTarget, ResultNode)
        End If

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
        Dim PositionStartY As Integer = CurrentToken.PositionStartY
        Dim PositionStartX As Integer = CurrentToken.PositionStartX
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
        Dim ResultNode As New ClassNode(PositionStartY, PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX, CurrentToken.Value, export, primary)
        If {"int", "float", "str", "bool", "any", "fun", "list"}.Contains(CurrentToken.Value) Then
            If Not ParentFile.filepath = executableDirectory & "/libs/std.lim" Then
                ThrowCoordinatesSyntaxLimException("ASTGC03", "The class """ & CurrentToken.Value & """ is already defined.", ParentFile, CurrentToken.PositionStartY, CurrentToken.PositionStartX, CurrentToken.PositionEndY, CurrentToken.PositionEndX)
            End If
            Select Case CurrentToken.Value
                Case "int"
                    Compiler.STDClass_int = ResultNode
                Case "float"
                    Compiler.STDClass_float = ResultNode
                Case "str"
                    Compiler.STDClass_str = ResultNode
                Case "bool"
                    Compiler.STDClass_bool = ResultNode
                Case "any"
                    Compiler.STDClass_any = ResultNode
                Case "fun"
                    Compiler.STDClass_fun = ResultNode
                Case "list"
                    Compiler.STDClass_list = ResultNode
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
        Dim HasCloneMethod As Boolean = False
        Dim HasStr As Boolean = False
        Dim HasRepr As Boolean = False
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
                Dim CastedMethod As FunctionNode = DirectCast(LineResult, FunctionNode)
                Select Case CastedMethod.FunctionName
                    Case "clone"
                        HasCloneMethod = True
                    Case "str"
                        HasStr = True
                    Case "repr"
                        HasRepr = True
                End Select
            ElseIf TypeOf LineResult Is RelationNode Then
                ResultNode.Relations.Add(LineResult)
            ElseIf TypeOf LineResult Is DeclareVariableNode Then
                ResultNode.DeclareVariables.Add(LineResult)
            ElseIf TypeOf LineResult Is AddSourceDirectlyStatementNode Then
                ResultNode.AddSourcesDirectly.Add(LineResult)
            Else
                ThrowCoordinatesSyntaxLimException("ASTGC07", "This was not expected here.", ParentFile, LineResult.PositionStartY, LineResult.PositionStartX, LineResult.PositionEndY, LineResult.PositionEndX, "Check line indentation.")
            End If


        End While

        'Primary & not clone
        If primary And Not HasCloneMethod Then
            ThrowCoordinatesSyntaxLimException("ASTGC09", "A class defined as ""primary"" must have a clone method.", ParentFile, ResultNode.PositionStartY, ResultNode.PositionStartX, ResultNode.PositionEndY, ResultNode.PositionEndX)
        End If

        'Str
        If Not HasStr Then
            ResultNode.Methods.Add(CreateMethod(ResultNode, "str", "str", "return new_str(""<object: " & ResultNode.ClassName & ">"");"))
        End If

        'Repr
        If Not HasRepr Then
            ResultNode.Methods.Add(CreateMethod(ResultNode, "repr", "str", "return {{str()}};"))
        End If

        'Return
        Return ResultNode

    End Function

    Private Function CreateMethod(ByVal Parentclass As ClassNode, ByVal name As String, ByVal returnType As String, ByVal line As String) As FunctionNode

        'Create ASD
        Dim ASD As New AddSourceDirectlyNode(-1, -1, -1, -1, line)
        Dim SASD As New AddSourceDirectlyStatementNode(ASD)
        Dim Method As New FunctionNode(-1, -1, -1, -1, name, False, New List(Of FunctionArgumentNode), New TypeNode(-1, -1, -1, -1, returnType))
        SASD.ParentNode = Method
        Method.ParentNode = Parentclass
        Method.Codes.Add(SASD)
        Return Method

    End Function

End Class
