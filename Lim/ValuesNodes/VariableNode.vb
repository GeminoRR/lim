'===================================
'========== VARIABLE NODE ==========
'===================================
'
' Represents a variable call
'
Class VariableNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public VariableName As String
    Private Var As Variable = Nothing
    Private PreText As String = ""

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal VariableName As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.VariableName = VariableName

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return VariableName
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get variable
        GetTarget()

        'Is a primary type
        If Var.ValueType.ParentClass.Primary Then
            For Each Method As FunctionNode In Var.ValueType.Methods
                If Method.FunctionName = "clone" Then
                    Method.Compile(Nothing)
                    Return Method.CompiledName & "(" & PreText & Var.CompiledName & ")"
                End If
            Next
            ThrowNodeSyntaxException("VNC01", "Unable to find ""clone"" method.", Me)
            Return ""
        Else
            Return PreText & Var.CompiledName
        End If

    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return False
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type

        'Get variable
        GetTarget()

        'Return type
        Return Var.ValueType

    End Function

    '======================================
    '========== REFRESH VARIABLE ==========
    '======================================
    Public Function GetTarget(Optional ByVal SearchFunction As Boolean = False) As FunctionNode

        'No need
        If Var IsNot Nothing And Not SearchFunction Then
            Return Nothing
        End If

        'Check variables from this file
        Dim UpperNode As Node = Me
        While UpperNode.ParentNode IsNot Nothing

            'Get uppernode
            UpperNode = UpperNode.ParentNode

            'No a scope
            If Not TypeOf UpperNode Is ScopeNode Then
                Continue While
            End If

            'Loop in each variable
            For Each ScopeVar As Variable In DirectCast(UpperNode, ScopeNode).Variables
                If ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar
                    If TypeOf UpperNode Is Type Then
                        PreText = "self->"
                    End If
                    Return Nothing
                End If
            Next

        End While

        'Check variables from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each ScopeVar As Variable In ImportedFile.Variables
                If ScopeVar.Export And ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar
                    Return Nothing
                End If
            Next
        Next

        'Current file DeclareVariables
        For Each ScopeVar As DeclareVariableNode In Me.ParentFile.DeclareVariables
            If ScopeVar.VariableName = Me.VariableName Then
                Var = ScopeVar.CompileFor(Compiled_Variables, Compiled_Inits)
                Return Nothing
            End If
        Next

        'Check DeclareVariables from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each ScopeVar As DeclareVariableNode In ImportedFile.DeclareVariables
                If ScopeVar.Export And ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar.CompileFor(Compiled_Variables, Compiled_Inits)
                    Return Nothing
                End If
            Next
        Next

        'Check functions from this file
        For Each fun As FunctionNode In Me.ParentFile.Functions
            If fun.FunctionName = Me.VariableName Then
                If SearchFunction Then
                    Return fun
                Else
                    Var = FunctionToVariable(fun)
                End If
                Return Nothing
            End If
        Next

        'Check Functions from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each fun As FunctionNode In ImportedFile.Functions
                If fun.Export And fun.FunctionName = Me.VariableName Then
                    If SearchFunction Then
                        Return fun
                    Else
                        Var = FunctionToVariable(fun)
                    End If
                    Return Nothing
                End If
            Next
        Next

        'Not find
        If SearchFunction Then
            Return Nothing
        End If
        ThrowNodeNamingException("CGV01", "Unable to find variable """ & Me.VariableName & """", Me)
        Return Nothing

    End Function

    '=================================================
    '========== FUNCTION TO GLOBAL VARIABLE ==========
    '=================================================
    Private Function FunctionToVariable(ByVal fun As FunctionNode) As Variable

        'Create the variable
        Dim NewVar As New Variable(fun.FunctionName, fun.MinimumFunctionType, True, "VariableOfFunction_" & fun.MinimumFunctionCompiledName, fun.Export)

        'Set variable
        Me.ParentFile.Variables.Add(NewVar)

        'Compile the variable
        Compiled_Variables.Add(fun.MinimumFunctionType.CompiledName & " * " & NewVar.CompiledName & ";")
        Compiled_Inits.Add(NewVar.CompiledName & " = " & fun.MinimumFunctionType.CompiledName & "_allocate();")
        Compiled_Inits.Add(NewVar.CompiledName & "->target = " & fun.MinimumFunctionCompiledName & ";")

        'Return
        Return NewVar

    End Function

End Class