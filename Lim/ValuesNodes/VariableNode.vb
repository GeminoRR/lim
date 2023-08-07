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

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As VariableNode = Me.MemberwiseClone()
        Cloned.Var = Nothing
        Return Cloned

    End Function

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

    '=================================
    '========== COMPILE REF ==========
    '=================================
    Public Function CompileRef(content As List(Of String)) As String

        'Get variable
        GetTarget(content)

        'Global variable
        If Me.Var.GlobalVariable Then
            PreText = "GV->"
        End If

        'Doesn't check primary type
        Return PreText & Var.CompiledName

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get variable
        GetTarget(content)

        'Global variable
        If Me.Var.GlobalVariable Then
            PreText = "GV->"
        End If

        'Is a primary type
        If Var.ValueType.ParentClass.Primary Then
            For Each Method As FunctionNode In Var.ValueType.Methods
                If Method.FunctionName = "clone" Then
                    Method.Compile(Nothing)
                    Return Method.CompiledName & "(GV, " & PreText & Var.CompiledName & ")"
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

        'Return type
        Return GetTarget()

    End Function

    '======================================
    '========== REFRESH VARIABLE ==========
    '======================================
    Public Function GetTarget(Optional ByVal content As List(Of String) = Nothing) As Object

        'Var already found
        If Var IsNot Nothing Then
            Return Var.ValueType
        End If

        'Check variables from this file
        Dim UpperNode As Node = Me
        Dim IsInRelation As Boolean = False
        While UpperNode.ParentNode IsNot Nothing

            'Get uppernode
            UpperNode = UpperNode.ParentNode

            'Is relation
            If TypeOf UpperNode Is RelationNode Then
                IsInRelation = True
            End If

            'No a scope
            If Not TypeOf UpperNode Is ScopeNode Then
                Continue While
            End If

            'Loop idn each variable
            For Each ScopeVar As Variable In DirectCast(UpperNode, ScopeNode).Variables
                If ScopeVar.VariableName = Me.VariableName Then
                    If TypeOf UpperNode Is Type Then
                        If Not IsInRelation Then
                            Var = ScopeVar
                            PreText = "self->"
                            Return Var.ValueType
                        End If
                    Else
                        Var = ScopeVar
                        Return Var.ValueType
                    End If
                End If
            Next

            'Loop in each method
            If TypeOf UpperNode Is Type And Not IsInRelation Then
                For Each method As FunctionNode In DirectCast(UpperNode, Type).Methods
                    If method.FunctionName = Me.VariableName Then
                        If content IsNot Nothing Then
                            Var = New Variable(method.FunctionName, method.MinimumFunctionType, True, GetFunctionCompiledName(), method.Export)
                            Me.ParentScope.Variables.Add(Var)
                            content.Add(method.MinimumFunctionType.CompiledName & " * " & Var.CompiledName & ";")
                            content.Add(Var.CompiledName & " = " & method.MinimumFunctionType.CompiledName & "_allocate(GV);")
                            content.Add(Var.CompiledName & "->target = " & method.MinimumFunctionCompiledName & ";")
                            content.Add(Var.CompiledName & "->object = self;")
                            Return Nothing
                        Else
                            Return method.MinimumFunctionType
                        End If
                    End If
                Next
            End If

        End While

        'Check variables from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each ScopeVar As Variable In ImportedFile.Variables
                If ScopeVar.Export And ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar
                    Return Var.ValueType
                End If
            Next
        Next

        'Current file DeclareVariables
        For Each ScopeVar As DeclareVariableNode In Me.ParentFile.DeclareVariables
            If ScopeVar.VariableName = Me.VariableName Then
                Var = ScopeVar.CompileFor(Compiled_Variables, Compiled_Inits)
                Return Var.ValueType
            End If
        Next

        'Check DeclareVariables from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each ScopeVar As DeclareVariableNode In ImportedFile.DeclareVariables
                If ScopeVar.Export And ScopeVar.VariableName = Me.VariableName Then
                    Var = ScopeVar.CompileFor(Compiled_Variables, Compiled_Inits)
                    Return Var.ValueType
                End If
            Next
        Next

        'Check functions from this file
        For Each fun As FunctionNode In Me.ParentFile.Functions
            If fun.FunctionName = Me.VariableName Then
                Var = FunctionToGlobalVariable(fun)
                PreText = "GV->"
                Return Var.ValueType
            End If
        Next

        'Check Functions from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each fun As FunctionNode In ImportedFile.Functions
                If fun.Export And fun.FunctionName = Me.VariableName Then
                    Var = FunctionToGlobalVariable(fun)
                    PreText = "GV->"
                    Return Var.ValueType
                End If
            Next
        Next

        'Not find
        ThrowNodeNamingException("CGV01", "Unable to find variable """ & Me.VariableName & """", Me)
        Return Nothing

    End Function

    '=================================================
    '========== FUNCTION TO GLOBAL VARIABLE ==========
    '=================================================
    Private Function FunctionToGlobalVariable(ByVal fun As FunctionNode) As Variable

        'Create the variable
        Dim NewVar As New Variable(fun.FunctionName, fun.MinimumFunctionType, True, "VariableOfFunction_" & fun.MinimumFunctionCompiledName, fun.Export)

        'Set variable
        Me.ParentFile.Variables.Add(NewVar)

        'Compile the variable
        Compiled_Variables.Add(fun.MinimumFunctionType.CompiledName & " * " & NewVar.CompiledName & ";")
        Compiled_Inits.Add("GV->" & NewVar.CompiledName & " = " & fun.MinimumFunctionType.CompiledName & "_allocate(GV);")
        Compiled_Inits.Add("GV->" & NewVar.CompiledName & "->target = " & fun.MinimumFunctionCompiledName & ";")

        'Return
        Return NewVar

    End Function

    '==================================
    '========== GET FUNCTION ==========
    '==================================
    Public Function GetFunction() As FunctionNode

        'Check variables from this file
        Dim UpperNode As Node = Me
        While UpperNode.ParentNode IsNot Nothing

            'Get uppernode
            UpperNode = UpperNode.ParentNode

            'Is in relation
            If TypeOf UpperNode Is RelationNode Then
                Exit While
            End If

            'No a scope
            If Not TypeOf UpperNode Is ScopeNode Then
                Continue While
            End If

            'Loop in each method
            If TypeOf UpperNode Is Type Then
                For Each method As FunctionNode In DirectCast(UpperNode, Type).Methods
                    If method.FunctionName = Me.VariableName Then
                        Return method
                    End If
                Next
            End If

        End While

        'Check functions from this file
        For Each fun As FunctionNode In Me.ParentFile.Functions
            If fun.FunctionName = Me.VariableName Then
                Return fun
            End If
        Next

        'Check Functions from imported files
        For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
            For Each fun As FunctionNode In ImportedFile.Functions
                If fun.Export And fun.FunctionName = Me.VariableName Then
                    Return fun
                End If
            Next
        Next

        'Not find
        Return Nothing

    End Function

End Class