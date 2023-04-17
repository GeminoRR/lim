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

        While TokenIndex < Tokens.Count

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


    '=============================
    '========= GET CLASS =========
    '=============================
    Private Function GetClass() As Node

    End Function

End Module
