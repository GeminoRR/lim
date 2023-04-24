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
        RefreshVariable()

        'Return
        Return PreText & Var.CompiledName

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
        RefreshVariable()

        'Return type
        Return Var.ValueType

    End Function

    '======================================
    '========== REFRESH VARIABLE ==========
    '======================================
    Private Sub RefreshVariable()

        'No need
        If Var IsNot Nothing Then
            Exit Sub
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
                    Exit Sub
                End If
            Next

        End While

        'Check variables from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each ScopeVar As Variable In ImportedFile.Variables
                If ScopeVar.Export And ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar
                    Exit Sub
                End If
            Next
        Next

        'Current file DeclareVariables
        For Each ScopeVar As DeclareVariableNode In Me.ParentFile.DeclareVariables
            If ScopeVar.VariableName = Me.VariableName Then
                Var = ScopeVar.CompileFor(Compiled_Variables, Compiled_Inits)
                Exit Sub
            End If
        Next

        'Check DeclareVariables from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each ScopeVar As DeclareVariableNode In ImportedFile.DeclareVariables
                If ScopeVar.Export And ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar.CompileFor(Compiled_Variables, Compiled_Inits)
                    Exit Sub
                End If
            Next
        Next

        'Check functions from this file
        For Each fun As FunctionNode In Me.ParentFile.Functions
            If fun.FunctionName = Me.VariableName Then
                Var = FunctionToVariable(Me, fun)
                Exit Sub
            End If
        Next

        'Check Functions from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each fun As FunctionNode In ImportedFile.Functions
                If fun.Export And fun.FunctionName = Me.VariableName Then
                    Var = FunctionToVariable(Me, fun)
                    Exit Sub
                End If
            Next
        Next

        'Not find
        ThrowNodeNamingException("CGV01", "Unable to find variable """ & Me.VariableName & """", Me)
        Exit Sub

    End Sub

    '==========================================
    '========== FUNCTION TO VARIABLE ==========
    '==========================================
    Private Function FunctionToVariable(ByVal CallerNode As Node, ByVal fun As FunctionNode) As Variable

        'Create the variable
        Dim NewVar As New Variable(fun.FunctionName, fun.FunctionType, True, "V_" & fun.CompiledName, fun.Export)
        CallerNode.ParentFile.Variables.Add(NewVar)

        'Compile the variable
        Compiled_Variables.Add(fun.FunctionType.CompiledName & " * " & NewVar.CompiledName & ";")
        Compiled_Inits.Add(NewVar.CompiledName & " = " & fun.FunctionType.CompiledName & "_allocate();")
        Compiled_Inits.Add(NewVar.CompiledName & "->target = " & fun.CompiledName & ";")

        'Return
        Return NewVar

    End Function

End Class