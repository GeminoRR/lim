Imports System.IO
'==============================
'========== COMPILER ==========
'==============================
'
' Main file.
'

Module Compiler

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly executableDirectory As String = Directory.GetParent(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName).FullName.Replace("\", "/")
    Public ReadOnly AppData As String = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData).Replace("\", "/") & "/Lim"

    Public Compiled_Imports As New List(Of String)

    Public Compiled_TypesPrototypes As New List(Of String)
    Public Compiled_FunctionsPrototypes As New List(Of String)

    Public Compiled_Variables As New List(Of String)

    Public Compiled_Types As New List(Of String)
    Public Compiled_Functions As New List(Of String)

    Public Compiled_Inits As New List(Of String)

    Public AllImportedFiles As New List(Of SourceFile)
    Public MainFile As SourceFile

    Public DefinedTypes As New List(Of Type)
    Public ClassCounter As Integer = 0

    Public STDClass_int As ClassNode = Nothing
    Public STD_int As Type = Nothing
    Public STDClass_float As ClassNode = Nothing
    Public STD_float As Type = Nothing
    Public STDClass_str As ClassNode = Nothing
    Public STD_str As Type = Nothing
    Public STDClass_bool As ClassNode = Nothing
    Public STD_bool As Type = Nothing
    Public STDClass_any As ClassNode = Nothing
    Public STD_any As Type = Nothing
    Public STDClass_fun As ClassNode = Nothing
    Public STDClass_list As ClassNode = Nothing

    '==================================
    '========== GET TEMPLATE ==========
    '==================================
    Public Function ReadTemplateFile(ByVal filepath As String) As String
        If Not File.Exists(filepath) Then
            ThrowSimpleLimException("CGT01", "Cannot read file", """" & filepath & """ is missing")
        End If
        Dim result As String = ""
        Try
            result = File.ReadAllText(filepath)
        Catch ex As Exception
            ThrowSimpleLimException("CGT02", "Cannot read file", ex.Message)
        End Try
        Return result
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Sub compile(ByVal inputFile As String)

        'Reset build environment
        Try

            'src directory
            If Not Directory.Exists(AppData & "/src") Then
                Directory.CreateDirectory(AppData & "/src")
            End If

            'Gc
            If Not Directory.Exists(AppData & "/src/gc") Then
                Directory.CreateDirectory(AppData & "/src/gc")
            End If
            'tgh.h
            If Not File.Exists(AppData & "/src/gc/tgc.h") Then
                File.Copy(executableDirectory & "/runtime/gc/tgc.h", AppData & "/src/gc/tgc.h", True)
            End If
            'tgh.c
            If Not File.Exists(AppData & "/src/gc/tgc.c") Then
                File.Copy(executableDirectory & "/runtime/gc/tgc.c", AppData & "/src/gc/tgc.c", True)
            End If

        Catch ex As Exception
            ThrowSimpleLimException("CC01", "Unable to reset build environment", ex.Message)
        End Try

        'Add basic import
        Compiled_Imports.Add("#include <stdio.h>")
        Compiled_Imports.Add("#include <stdlib.h>")
        Compiled_Imports.Add("#include <string.h>")
        Compiled_Imports.Add("#include <stdbool.h>")
        Compiled_Imports.Add("#include <time.h>")
        Compiled_Imports.Add("#include <math.h>")
        Compiled_Imports.Add("#include ""./gc/tgc.h""")

        'Add main file
        MainFile = New SourceFile(inputFile)
        AllImportedFiles.Add(MainFile)

        'Get main functions
        Dim MainFunction As FunctionNode = Nothing
        For Each fun As FunctionNode In MainFile.Functions
            If fun.FunctionName = "main" Then
                MainFunction = fun
                Exit For
            End If
        Next
        If MainFunction Is Nothing Then
            ThrowSimpleLimException("CC02", "Missing function", "The <" & MainFile.filename & "> file does not contain an entry point (main function).")
        End If
        If MainFunction.FunctionArguments.Count > 0 Then
            ThrowNodeSyntaxException("CC03", "The entry point cannot have any arguments.", MainFunction.FunctionArguments(0))
        End If
        If MainFunction.ReturnTypeNode IsNot Nothing Then
            ThrowNodeSyntaxException("CC04", "The entry point cannot return a value.", MainFunction.ReturnTypeNode)
        End If

        'Get Primary types
        If STDClass_int Is Nothing Then
            ThrowSimpleLimException("CC05", "Class missing", "Could not find class ""int"". Check the integrity of the ""std.lim"" library.")
        End If
        STD_int = New Type(STDClass_int, New List(Of Type), False)

        If STDClass_float Is Nothing Then
            ThrowSimpleLimException("CC06", "Class missing", "Could not find class ""float"". Check the integrity of the ""std.lim"" library.")
        End If
        STD_float = New Type(STDClass_float, New List(Of Type), False)

        If STDClass_str Is Nothing Then
            ThrowSimpleLimException("CC07", "Class missing", "Could not find class ""str"". Check the integrity of the ""std.lim"" library.")
        End If
        STD_str = New Type(STDClass_str, New List(Of Type), False)

        If STDClass_bool Is Nothing Then
            ThrowSimpleLimException("CC08", "Class missing", "Could not find class ""bool"". Check the integrity of the ""std.lim"" library.")
        End If
        STD_bool = New Type(STDClass_bool, New List(Of Type), False)

        If STDClass_any Is Nothing Then
            ThrowSimpleLimException("CC09", "Class missing", "Could not find class ""any"". Check the integrity of the ""std.lim"" library.")
        End If
        STD_any = New Type(STDClass_any, New List(Of Type), False)

        If STDClass_fun Is Nothing Then
            ThrowSimpleLimException("CC10", "Class missing", "Could not find class ""fun"". Check the integrity of the ""std.lim"" library.")
        End If

        If STDClass_list Is Nothing Then
            ThrowSimpleLimException("CC11", "Class missing", "Could not find class ""list"". Check the integrity of the ""std.lim"" library.")
        End If

        'Compile primary types
        STD_int.Compile(Nothing)
        STD_float.Compile(Nothing)
        STD_str.Compile(Nothing)
        STD_bool.Compile(Nothing)
        STD_any.Compile(Nothing)


        'Compile main
        MainFunction.Compile(Nothing)

        'Compile ASD Variables & ASD Functions
        Dim Compiled_AddSourceDirectly As New List(Of String)
        For Each ImportedFile As SourceFile In AllImportedFiles
            For Each ASD As AddSourceDirectlyStatementNode In ImportedFile.AddSourceDirectlys
                ASD.Compile(Compiled_AddSourceDirectly)
            Next
            For Each Fun As FunctionNode In ImportedFile.Functions
                If Not Fun.CustomHeader = "" Then
                    Fun.Compile(Nothing)
                End If
            Next
        Next

        'Generate final file
        File.WriteAllText(AppData & "/src/main.c", "", Text.Encoding.UTF8)
        Using SW As New StreamWriter(AppData & "/src/main.c", True)

            'Informations
            SW.WriteLine("/*")
            SW.WriteLine(vbTab & "File automatically generated by lim.")
            SW.WriteLine(vbTab & "Lim By Gémino Ruffault--Ravenel")
            SW.WriteLine("*/")

            'Imports
            SW.WriteLine("")
            SW.WriteLine("/////////////////////")
            SW.WriteLine("////// IMPORTS //////")
            SW.WriteLine("/////////////////////")
            For Each line As String In Compiled_Imports
                SW.WriteLine(line)
            Next

            'Types prototypes
            SW.WriteLine("")
            SW.WriteLine("/////////////////////////////")
            SW.WriteLine("////// TYPE PROTOTYPES //////")
            SW.WriteLine("/////////////////////////////")
            For Each line As String In Compiled_TypesPrototypes
                SW.WriteLine(line)
            Next

            'Functions prototypes
            SW.WriteLine("")
            SW.WriteLine("/////////////////////////////////")
            SW.WriteLine("////// FUNCTION PROTOTYPES //////")
            SW.WriteLine("/////////////////////////////////")
            For Each line As String In Compiled_FunctionsPrototypes
                SW.WriteLine(line)
            Next

            'Add Source Directly
            SW.WriteLine("")
            SW.WriteLine("/////////////////////////////////")
            SW.WriteLine("////// ADD SOURCE DIRECTLY //////")
            SW.WriteLine("/////////////////////////////////")
            For Each line As String In Compiled_AddSourceDirectly
                SW.WriteLine(line)
            Next

            'Variables
            SW.WriteLine("")
            SW.WriteLine("///////////////////////")
            SW.WriteLine("////// VARIABLES //////")
            SW.WriteLine("///////////////////////")
            SW.WriteLine("static tgc_t gc;")
            For Each line As String In Compiled_Variables
                SW.WriteLine(line)
            Next

            'Types
            SW.WriteLine("")
            SW.WriteLine("/////////////////////////////")
            SW.WriteLine("////// TYPES (Classes) //////")
            SW.WriteLine("/////////////////////////////")
            For Each line As String In Compiled_Types
                SW.WriteLine(line)
            Next

            'Functions
            SW.WriteLine("")
            SW.WriteLine("///////////////////////")
            SW.WriteLine("////// FUNCTIONS //////")
            SW.WriteLine("///////////////////////")
            For Each line As String In Compiled_Functions
                SW.WriteLine(line)
            Next

            'Entry point
            SW.WriteLine("")
            SW.WriteLine("/////////////////////////")
            SW.WriteLine("////// ENTRY POINT //////")
            SW.WriteLine("/////////////////////////")
            SW.WriteLine("int main(int argc, char **argv){")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Initialize the random")
            SW.WriteLine(vbTab & "srand(time(NULL));")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Start the garbage collector")
            SW.WriteLine(vbTab & "tgc_start(&gc, &argc);")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Initialize global variable values")
            For Each line As String In Compiled_Inits
                SW.WriteLine(vbTab & line)
            Next
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Calls the ""main"" function")
            SW.WriteLine(vbTab & MainFunction.CompiledName & "(NULL);")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Stop the garbage collector")
            SW.WriteLine(vbTab & "tgc_stop(&gc);")
            SW.WriteLine("}")

        End Using


    End Sub

    '==============================
    '========== GET TYPE ==========
    '==============================
    Public Function GetTypeFromClassAndArgs(ByVal CallerNode As Node, ByVal TargetClass As ClassNode, Optional ByVal Arguments As List(Of Type) = Nothing) As Type

        'Arg
        If Arguments Is Nothing Then
            Arguments = New List(Of Type)
        End If

        'Type already exist
        For Each AlreadyDefinedType As Type In DefinedTypes

            'Not the same class
            If (Not AlreadyDefinedType.ParentClass = TargetClass) Then
                Continue For
            End If

            'Argument count
            If (Not Arguments.Count = AlreadyDefinedType.PassedArguments.Count) Then
                If AlreadyDefinedType.ParentClass.ClassName = "fun" Then
                    Continue For
                End If
                ThrowNodeTypeException("CGTFCAA01", "The number of arguments given does not correspond to the number of arguments requested by the class.", CallerNode)
            End If

            'Arguments types
            Dim AllArgsAreTheSames As Boolean = True
            For i As Integer = 0 To Arguments.Count - 1
                If Not AlreadyDefinedType.PassedArguments(i) = Arguments(i) Then
                    AllArgsAreTheSames = False
                    Exit For
                End If
            Next

            'Type trouver
            If AllArgsAreTheSames Then
                Return AlreadyDefinedType
            End If

        Next

        'Not find
        Return New Type(TargetClass, Arguments)

    End Function

    '=======================================
    '========== GET COMPILED NAME ==========
    '=======================================
    Private Function GenerateUniqueName(ByVal nombre As Integer) As String

        Dim caracteres As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

        'Dim chaine As String = ""
        'Dim rnd As New Random()

        '' Générer une chaîne de 10 caractères aléatoires
        'For i As Integer = 1 To 10
        '    Dim index As Integer = rnd.Next(0, caracteres.Length)
        '    chaine += caracteres(index)
        'Next

        '' Ajouter le nombre à la fin de la chaîne
        'chaine += nombre.ToString()

        '' Retourner la chaîne générée
        'Return chaine

        Dim colones As New List(Of Integer) From {-1}
        For i As Integer = 0 To nombre

            colones(colones.Count - 1) += 1
            For x As Integer = 0 To colones.Count - 1
                Dim currentColoneCheck As Integer = colones.Count - 1 - x
                If colones(currentColoneCheck) >= caracteres.Count Then
                    colones(currentColoneCheck) = 0
                    If currentColoneCheck = 0 Then
                        colones.Add(0)
                    Else
                        colones(currentColoneCheck - 1) += 1
                    End If
                End If
            Next
        Next

        Dim str As String = ""
        For Each col As Integer In colones
            str &= caracteres(col)
        Next
        Return str

    End Function


    Private FunctionsCount As Integer = 0
    Public Function GetFunctionCompiledName() As String
        FunctionsCount += 1
        Return "F_" & GenerateUniqueName(FunctionsCount)
    End Function


    Private MethodCount As Integer = 0
    Public Function GetMethodCompiledName() As String
        MethodCount += 1
        Return "M_" & GenerateUniqueName(MethodCount)
    End Function


    Private RelationCount As Integer = 0
    Public Function GetRelationCompiledName() As String
        RelationCount += 1
        Return "R_" & GenerateUniqueName(RelationCount)
    End Function


    Private TypeCount As Integer = 0
    Public Function GetTypeCompiledName() As String
        TypeCount += 1
        Return "T_" & GenerateUniqueName(TypeCount)
    End Function


    Private VariablesCount As Integer = 0
    Public Function GetVariableCompiledName() As String
        VariablesCount += 1
        Return "V_" & GenerateUniqueName(VariablesCount)
    End Function

End Module