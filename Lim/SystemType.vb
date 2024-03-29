﻿'==========================
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
    Public ReadOnly AddSourcesDirectly As New List(Of AddSourceDirectlyStatementNode)
    Public ReadOnly DeclareVariables As New List(Of DeclareVariableNode)
    Public ReadOnly TypeID As Integer

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal ParentClass As ClassNode, ByVal PassedArguments As List(Of Type), Optional CompileNow As Boolean = True)

        MyBase.New(ParentClass.PositionStartY, ParentClass.PositionStartX, ParentClass.PositionEndY, ParentClass.PositionEndX)
        TypeIDCounter += 1
        TypeID = TypeIDCounter
        Me.ParentClass = ParentClass
        Me.ParentNode = Me.ParentClass.ParentNode
        Me.PassedArguments = PassedArguments
        If Me.ParentClass.SoloType = Nothing Then
            Me.CompiledName = GetTypeCompiledName()
        Else
            Me.CompiledName = Me.ParentClass.SoloType
        End If
        For Each ASD As AddSourceDirectlyStatementNode In ParentClass.AddSourcesDirectly
            Me.AddSourcesDirectly.Add(ASD.Clone(Me))
        Next
        For Each DeclareVariable As DeclareVariableNode In ParentClass.DeclareVariables
            Me.DeclareVariables.Add(DeclareVariable.Clone(Me))
        Next
        For Each Method As FunctionNode In ParentClass.Methods
            Me.Methods.Add(Method.Clone(Me))
        Next
        For Each Relation As RelationNode In ParentClass.Relations
            Me.Relations.Add(Relation.Clone(Me))
        Next

        Compiler.DefinedTypes.Add(Me)

        If CompileNow Then
            Compile(Nothing)
        End If

    End Sub

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Throw New NotImplementedException()

    End Function

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

        'Allocate
        Dim AllocateContent As New List(Of String)
        AllocateContent.Add("")
        AllocateContent.Add("//Allocate memory")
        AllocateContent.Add(Me.CompiledName & " * self = tgc_alloc(&gc, sizeof(" & Me.CompiledName & "));")
        AllocateContent.Add("if (self == NULL){")
        AllocateContent.Add(vbTab & "ThrowRuntimeError(""Not enough memory."");")
        AllocateContent.Add("}")

        'Propertie comment
        If Me.DeclareVariables.Count > 0 Then
            AllocateContent.Add("")
            AllocateContent.Add("//Initialize default values of properties")
        End If

        'Typedef
        Dim TypeDefContent As New List(Of String)
        For Each ASD As AddSourceDirectlyStatementNode In Me.AddSourcesDirectly
            ASD.Compile(TypeDefContent)
        Next
        For Each DeclareVariable As DeclareVariableNode In Me.DeclareVariables
            Me.Variables.Add(DeclareVariable.CompileFor(TypeDefContent, AllocateContent, True))
        Next

        'Typedef for functions
        If Me.ParentClass.ClassName = "fun" Then

            'Arguments
            Dim FunArguments As String = "global_variables*, void*"
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

        'Return
        AllocateContent.Add("")
        AllocateContent.Add("//Return")
        AllocateContent.Add("return self;")

        'Compile typedef
        If Me.ParentClass.SoloType = Nothing Then

            'Typedef & Struct prototype
            Compiled_TypesPrototypes.Add("/* " & Me.ToString() & " */ typedef struct " & Me.CompiledName & " " & Me.CompiledName & ";")

            'Typedef & Struct
            Compiled_Types.Add("")
            Compiled_Types.Add("")
            Compiled_Types.Add("")
            Compiled_Types.Add("//" & Me.ToString())
            Compiled_Types.Add("typedef struct " & Me.CompiledName & "{")
            For Each line As String In TypeDefContent
                Compiled_Types.Add(vbTab & line)
            Next
            Compiled_Types.Add("} " & Me.CompiledName & ";")

        End If

        'Allocate
        Compiled_FunctionsPrototypes.Add("/* " & Me.ToString() & " -> allocate */ " & Me.CompiledName & " * " & Me.CompiledName & "_allocate(global_variables * GV);")
        Compiled_Types.Add("")
        Compiled_Types.Add("//Allocate")
        Compiled_Types.Add(Me.CompiledName & " * " & Me.CompiledName & "_allocate(global_variables * GV){")
        For Each line As String In AllocateContent
            Compiled_Types.Add(vbTab & line)
        Next
        Compiled_Types.Add("")
        Compiled_Types.Add("}")

    End Sub

    '=================================
    '========== IS ITERABLE ==========
    '=================================
    Public ReadOnly Property Iterators As Tuple(Of RelationNode, RelationNode, RelationNode)
        Get
            If _IsIterable Is Nothing Then
                Dim HasFrom As RelationNode = Nothing
                Dim HasTo As RelationNode = Nothing
                Dim HasIteration As RelationNode = Nothing
                For Each relation As RelationNode In Me.Relations
                    If relation.RelationOperator = RelationOperator.FOR_FROM Then
                        HasFrom = relation
                    ElseIf relation.RelationOperator = RelationOperator.FOR_TO Then
                        HasTo = relation
                    ElseIf relation.RelationOperator = RelationOperator.FOR_ITERATION Then
                        HasIteration = relation
                    End If
                Next
                If HasFrom IsNot Nothing And HasTo IsNot Nothing And HasIteration IsNot Nothing Then
                    _IsIterable = (HasFrom, HasTo, HasIteration).ToTuple()
                    HasFrom.Compile(Nothing)
                    HasTo.Compile(Nothing)
                    HasIteration.Compile(Nothing)
                Else
                    _IsIterable = Nothing
                End If
            End If
            Return _IsIterable
        End Get
    End Property
    Private _IsIterable As Tuple(Of RelationNode, RelationNode, RelationNode) = Nothing

End Class
