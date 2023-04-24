'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Class Type
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly CompiledName As String
    Public ReadOnly ParentClass As ClassNode
    Public ReadOnly PassedArguments As List(Of Type)
    Public ReadOnly Methods As New List(Of FunctionNode)
    Public ReadOnly Relations As New List(Of RelationNode)

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ParentClass As ClassNode, ByVal PassedArguments As List(Of Type), Optional CompileNow As Boolean = True)

        MyBase.New(ParentClass.PositionStartY, ParentClass.PositionStartX, ParentClass.PositionEndY, ParentClass.PositionEndX)

        Me.ParentClass = ParentClass
        Me.ParentNode = Me.ParentClass.ParentNode
        Me.PassedArguments = PassedArguments
        Me.CompiledName = GetTypeCompiledName()

        For Each Method As FunctionNode In ParentClass.Methods
            Dim ClonedMethod As FunctionNode = Method.Clone()
            ClonedMethod.ParentNode = Me
            Me.Methods.Add(ClonedMethod)
        Next
        For Each Relation As RelationNode In ParentClass.Relations
            Dim ClonedRelation As RelationNode = Relation.Clone()
            ClonedRelation.ParentNode = Me
            Me.Relations.Add(ClonedRelation)
        Next

        Compiler.DefinedTypes.Add(Me)

        If CompileNow Then
            Compile(Nothing)
        End If

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Arguments As String = ""
        If Me.PassedArguments.Count > 0 Then
            If Me.ParentClass.ClassName = "fun" Then
                For i As Integer = 1 To Me.PassedArguments.Count - 1
                    Arguments &= ", " & Me.PassedArguments(i).ToString()
                Next
                If Arguments.StartsWith(", ") Then
                    Arguments = Arguments.Substring(2)
                End If
                Arguments = "<" & Arguments & ">"
                If Me.PassedArguments(0) IsNot Nothing Then
                    Arguments &= "<" & Me.PassedArguments(0).ToString() & ">"
                Else
                    Arguments &= "<>"
                End If
            Else
                For Each arg As Type In Me.PassedArguments
                    Arguments &= ", " & arg.ToString()
                Next
                Arguments = "<" & Arguments.Substring(2) & ">"
            End If
        End If
        Return Me.ParentClass.ClassName & Arguments
    End Function

    '====================================
    '========== EQUAL OPERATOR ==========
    '====================================
    Shared Operator =(ByVal a As Type, ByVal b As Type)
        If a Is Nothing Or b Is Nothing Then
            Return a Is Nothing And b Is Nothing
        End If
        Return a.CompiledName = b.CompiledName
    End Operator

    Shared Operator <>(ByVal a As Type, ByVal b As Type)
        If a Is Nothing Or b Is Nothing Then
            Return False
        End If
        Return Not a.CompiledName = b.CompiledName
    End Operator

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(ByVal Content As List(Of String))

        'Struct
        Compiled_TypesPrototypes.Add("/* " & Me.ToString() & " */ typedef struct " & Me.CompiledName & " " & Me.CompiledName & ";")

        'Allocate
        Dim AllocateContent As New List(Of String)
        AllocateContent.Add("")
        AllocateContent.Add("//Allocate memory")
        AllocateContent.Add(Me.CompiledName & " * self = tgc_alloc(&gc, sizeof(" & Me.CompiledName & "));")
        AllocateContent.Add("if (self == NULL){")
        AllocateContent.Add(vbTab & "ThrowRuntimeError(""Not enough memory."");")
        AllocateContent.Add("}")
        AllocateContent.Add("")
        AllocateContent.Add("//Initialize default values of properties")

        'Typedef
        Dim TypeDefContent As New List(Of String)
        For Each ASD As AddSourceDirectlyStatementNode In Me.ParentClass.AddSourcesDirectly
            ASD.Compile(TypeDefContent)
        Next
        For Each DeclareVariable As DeclareVariableNode In Me.ParentClass.DeclareVariables
            Me.Variables.Add(DeclareVariable.CompileFor(TypeDefContent, AllocateContent))
        Next

        'Typedef for fun
        If Me.ParentClass.ClassName = "fun" Then

            'Arguments
            Dim FunArguments As String = "void*"
            For i As Integer = 1 To Me.PassedArguments.Count - 1
                FunArguments &= ", " & Me.PassedArguments(i).CompiledName & "*"
            Next
            FunArguments = "(" & FunArguments & ")"

            'Return type
            Dim FunReturnType As String
            If Me.PassedArguments(0) Is Nothing Then
                FunReturnType = "void"
            Else
                FunReturnType = Me.PassedArguments(0).CompiledName
            End If

            TypeDefContent.Add(FunReturnType & " * (*target)" & FunArguments & ";")
            AllocateContent.Add("self->target = NULL;")
            AllocateContent.Add("self->object = NULL;")

        End If

        'Compile typedef
        Compiled_Types.Add("")
        Compiled_Types.Add("")
        Compiled_Types.Add("")
        Compiled_Types.Add("//" & Me.ToString())
        Compiled_Types.Add("typedef struct " & Me.CompiledName & "{")
        For Each line As String In TypeDefContent
            Compiled_Types.Add(vbTab & line)
        Next
        Compiled_Types.Add("} " & Me.CompiledName & ";")

        'Allocate
        Compiled_FunctionsPrototypes.Add("/* " & Me.ToString() & " -> allocate */ " & Me.CompiledName & " * " & Me.CompiledName & "_allocate();")
        Compiled_Types.Add("")
        Compiled_Types.Add("//Allocate")
        Compiled_Types.Add(Me.CompiledName & " * " & Me.CompiledName & "_allocate(){")
        For Each line As String In AllocateContent
            Compiled_Types.Add(vbTab & line)
        Next
        Compiled_Types.Add("")
        Compiled_Types.Add("}")


    End Sub

End Class
