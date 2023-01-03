Imports System.IO
Public Class C_Compiler

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public files As New List(Of LimFile)
    Private entryFile As LimFile

    Private compiledImports As New List(Of String)
    Private compiledStructsPrototypes As New List(Of String)
    Private compiledFunctionsPrototypes As New List(Of String)
    Private compiledVariables As New List(Of String)
    Private compiledClasss As New List(Of String)
    Private compiledFunctions As New List(Of String)

    Private CompiledNameAlphabet As String = "abcdefghijklmnoqrstuvwxyz"
    Private variableCount As Integer = 0
    Private helpVariableCount As Integer = 0
    Private functionCount As Integer = 0
    Private classCount As Integer = 0

    Private compiledTypes As List(Of typeNode)

    Private stdInt As typeNode
    Private stdFloat As typeNode
    Private stdStr As typeNode
    Private stdBool As typeNode

    Private logs As Boolean = False

    '==================================
    '========== COMPILE CODE ==========
    '==================================
    Public Sub compileCode(ByVal inputFile As String, flags As List(Of String))

        'Restore variables
        compiledImports.Clear()
        compiledImports.Add("#include <stdio.h>")
        compiledImports.Add("#include <stdlib.h>")
        compiledImports.Add("#include <string.h>")
        compiledImports.Add("#include <stdbool.h>")
        compiledImports.Add("#include <math.h>")
        compiledImports.Add("#include ""tgc.h""")
        compiledStructsPrototypes.Clear()
        compiledFunctionsPrototypes.Clear()
        compiledVariables.Clear()
        compiledClasss.Clear()
        compiledFunctions.Clear()
        compiledTypes.Clear()
        variableCount = 0
        helpVariableCount = 0
        functionCount = 0
        classCount = 0
        Me.logs = flags.Contains("-l") Or flags.Contains("--logs")
        Dim outputFolder As String = AppData & "/compiled"

        'Get template
        Dim cTemplates As String = templateFolder & "/c"
        If Not Directory.Exists(cTemplates) Then
            addBasicError("Folder not found", "The ""templates/c"" folder could not be found. Try reinstalling lim.")
        End If

        'Add dimensions
        'TODO: Add dimensions load

        'Analyse first file
        entryFile = New LimFile(inputFile, Me)
        files.Add(entryFile)
        If logs Then
            addLog("File analysis: OK")
        End If

        'Get entry point
        Dim entryPoint As FunctionNode = Nothing
        For Each fun As FunctionNode In entryFile.functions
            If fun.Name = "main" Then
                entryPoint = fun
                Exit For
            End If
        Next
        If entryPoint Is Nothing Then
            addSyntaxError("VBCN01", "Entry point does not exist", files(0), -1, -1, "Add a ""main"" function")
        End If

        'Get std's types
        stdInt = New typeNode(-1, -1, "int", New List(Of typeNode))
        stdInt.targetClass = getClass("__int__", entryFile)
        stdFloat = New typeNode(-1, -1, "float", New List(Of typeNode))
        stdFloat.targetClass = getClass("__float__", entryFile)
        stdStr = New typeNode(-1, -1, "str", New List(Of typeNode))
        stdStr.targetClass = getClass("__str__", entryFile)
        stdBool = New typeNode(-1, -1, "bool", New List(Of typeNode))
        stdBool.targetClass = getClass("__bool__", entryFile)

        'Compiles types
        compileTypeNode(stdInt)
        compileTypeNode(stdFloat)
        compileTypeNode(stdStr)
        compileTypeNode(stdBool)

        'Logs
        If logs Then
            addLog("Load the ""std"" library: OK")
        End If

        'Compile start
        compileFunction(entryPoint)

        'Get final code
        Dim finalCode As String = ""

        'Imports
        finalCode = "/////////////////////////" & Environment.NewLine
        finalCode &= "//////// IMPORTS ////////" & Environment.NewLine
        finalCode &= "/////////////////////////"
        For Each line As String In compiledImports
            finalCode &= Environment.NewLine & line
        Next
        finalCode &= Environment.NewLine & Environment.NewLine

        'Structs Prototypes
        finalCode &= "////////////////////////////////////" & Environment.NewLine
        finalCode &= "//////// STRUCTS PROTOTYPES ////////" & Environment.NewLine
        finalCode &= "////////////////////////////////////"
        For Each line As String In compiledStructsPrototypes
            finalCode &= Environment.NewLine & line
        Next
        finalCode &= Environment.NewLine & Environment.NewLine

        'Functions Prototypes
        finalCode &= "//////////////////////////////////////" & Environment.NewLine
        finalCode &= "//////// FUNCTIONS PROTOTYPES ////////" & Environment.NewLine
        finalCode &= "//////////////////////////////////////"
        For Each line As String In compiledFunctionsPrototypes
            finalCode &= Environment.NewLine & line
        Next
        finalCode &= Environment.NewLine & Environment.NewLine

        'AddSourceDirectly
        finalCode &= "/////////////////////////////////////" & Environment.NewLine
        finalCode &= "//////// ADD SOURCE DIRECTLY ////////" & Environment.NewLine
        finalCode &= "/////////////////////////////////////"
        For Each file As LimFile In files

            If file.LimLib Then

                'File has sources variables
                If file.addSourceDirectly.Count > 0 Then

                    finalCode &= Environment.NewLine & "//" & file.name
                    For Each source As AddSourceNode In file.addSourceDirectly
                        finalCode &= Environment.NewLine & source.value
                    Next

                End If

            End If

        Next
        finalCode &= Environment.NewLine & Environment.NewLine

        'Variables
        finalCode &= "///////////////////////////" & Environment.NewLine
        finalCode &= "//////// VARIABLES ////////" & Environment.NewLine
        finalCode &= "///////////////////////////"
        finalCode &= Environment.NewLine & "static tgc_t gc;"
        For Each line As String In compiledVariables
            finalCode &= Environment.NewLine & line
        Next

        'Classs
        For Each line As String In compiledClasss
            finalCode &= Environment.NewLine & line
        Next

        'Functions
        For Each line As String In compiledFunctions
            finalCode &= Environment.NewLine & line
        Next
        finalCode &= Environment.NewLine & Environment.NewLine

        'Entry Point
        finalCode &= "/////////////////////////////////" & Environment.NewLine
        finalCode &= "////////// ENTRY POINT //////////" & Environment.NewLine
        finalCode &= "/////////////////////////////////" & Environment.NewLine
        finalCode &= "int main(int argc, char **argv){" & Environment.NewLine
        finalCode &= vbTab & "tgc_start(&gc, &argc);" & Environment.NewLine
        finalCode &= vbTab & entryPoint.compiledName & "();" & Environment.NewLine
        finalCode &= vbTab & "tgc_stop(&gc);" & Environment.NewLine
        finalCode &= "}"


        If logs Then
            addLog("Compile the program: OK")
        End If

        'Reload compile environment
        Try
            If Directory.Exists(outputFolder) Then
                Directory.Delete(outputFolder, True)
            End If
            Directory.CreateDirectory(outputFolder)
        Catch ex As Exception
            addBasicError("Cannot reload directory", ex.Message)
        End Try

        'Write file
        Try
            File.WriteAllText(outputFolder & "/main.c", finalCode)
        Catch ex As Exception
            addBasicError("Cannot write file", ex.Message)
        End Try
        If logs Then
            addLog("Writing the final file: OK")
        End If

    End Sub

    '===================================
    '========== GENERATE NAME ==========
    '===================================
    Private Function generateCompiledName(ByRef counter As Integer) As String

        counter += 1

        Dim colones As New List(Of Integer) From {-1}
        For i As Integer = 0 To counter

            colones(colones.Count - 1) += 1
            For x As Integer = 0 To colones.Count - 1
                Dim currentColoneCheck As Integer = colones.Count - 1 - x
                If colones(currentColoneCheck) >= CompiledNameAlphabet.Count Then
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
            str &= CompiledNameAlphabet(col)
        Next
        Return str

    End Function

    '=================================================
    '========== GENERATE HELP VARIABLE NAME ==========
    '=================================================
    Private Function getHelpVariableName() As String
        Return "hv_" & generateCompiledName(helpVariableCount)
    End Function

    '============================================
    '========== GENERATE VARIABLE NAME ==========
    '============================================
    Private Function getVariableName() As String
        Return "v_" & generateCompiledName(variableCount)
    End Function

    '============================================
    '========== GENERATE FUNCTION NAME ==========
    '============================================
    Private Function getFunctionName() As String
        Return "f_" & generateCompiledName(functionCount)
    End Function

    '=========================================
    '========== GENERATE CLASS NAME ==========
    '=========================================
    Private Function getClassName() As String
        Return "c_" & generateCompiledName(classCount)
    End Function

    '==========================================
    '========== VALUE IS INDEPENDENT ==========
    '==========================================
    Private Function ValueIsIndependent(ByVal value As Node) As Boolean

        'Value
        If TypeOf value Is valueNode Then
            Return True
        End If

        'String
        If TypeOf value Is StringNode Then
            Return True
        End If

        'Boolean
        If TypeOf value Is BooleanNode Then
            Return True
        End If

        'UnaryOp
        If TypeOf value Is UnaryOpNode Then

            'Cast
            Dim castedNode As UnaryOpNode = DirectCast(value, UnaryOpNode)

            'Get value
            Return ValueIsIndependent(castedNode.node)

        End If

        'ListNode
        If TypeOf value Is ListNode Then

            'Cast
            Dim castedNode As ListNode = DirectCast(value, ListNode)

            'Loop each element
            For Each elm As Node In castedNode.elements
                If Not ValueIsIndependent(elm) Then
                    Return False
                End If
            Next

            'Get value
            Return True

        End If

        'ListMap
        If TypeOf value Is MapNode Then

            'Cast
            Dim castedNode As MapNode = DirectCast(value, MapNode)

            'Loop each element
            For Each elm As List(Of Node) In castedNode.elements
                If Not ValueIsIndependent(elm(1)) Then
                    Return False
                End If
            Next

            'Get value
            Return True

        End If

        'Return
        Return False

    End Function

    '===============================
    '========== GET CLASS ==========
    '===============================
    Public Function getClass(ByVal name As String, ByVal nodeCaller As Node) As ClassNode

        'Get file
        Dim currentFile As LimFile = getNodeParentFile(nodeCaller)

        'Search in Current file
        For Each currentClass As ClassNode In currentFile.classs
            If currentClass.Name = name Then
                Return currentClass
            End If
        Next

        'Search in others files
        For Each file As LimFile In currentFile.FilesImports
            For Each currentClass As ClassNode In file.classs
                If currentClass.Name = name And currentClass.export Then
                    Return currentClass
                End If
            Next
        Next

        'Anable to find the class
        addNodeNamingError("VBCGC01", "The class """ & name & """ could not be found.", nodeCaller, "Check the class name or that the file is correctly imported.")
        Return Nothing

    End Function

    '==================================
    '========== GET VARIABLE ==========
    '==================================
    Public Function getVariable(ByVal name As String, ByVal nodeCaller As Node) As Variable

        'Get file
        Dim currentFile As LimFile = getNodeParentFile(nodeCaller)

        'Search in Current file
        Dim currentBlock As Node = nodeCaller
        While Not currentBlock Is Nothing

            If TypeOf currentBlock Is containerNode Then
                For Each currentVariable As Variable In DirectCast(currentBlock, containerNode).variables
                    If currentVariable.name = name Then
                        Return currentVariable
                    End If
                Next
            End If
            If TypeOf currentBlock Is ClassNode Then
                For Each currentVariable As Variable In DirectCast(currentBlock, ClassNode).variables
                    If currentVariable.name = name Then
                        Return currentVariable
                    End If
                Next
            End If

            currentBlock = currentBlock.parentNode

        End While

        'Search in current file variables
        For Each var As Variable In currentFile.variables
            If var.name = name Then
                Return var
            End If
        Next
        For Each declareVariable As DeclareVariableNode In currentFile.declareVariables

            'Is the one that we search ?
            If Not declareVariable.variableName = name Then
                Continue For
            End If

            'TODO: COMPILE DECLARE VARIABLE

        Next

        'Search in others files
        For Each file As LimFile In currentFile.FilesImports

            'Get variables
            For Each var As Variable In file.variables
                If var.name = name Then
                    Return var
                End If
            Next

            'Declare variables
            For Each declareVariable As DeclareVariableNode In file.declareVariables

                'Is the one that we search ?
                If Not (declareVariable.variableName = name And declareVariable.export) Then
                    Continue For
                End If

                'TODO: COMPILE DECLARE VARIABLE

            Next

        Next

        'Anable to find the class
        addNodeNamingError("VBCGV01", "The variable """ & name & """ could not be found.", nodeCaller, "Check the variable name or that the file is correctly imported.")
        Return Nothing

    End Function

    '==================================
    '========== GET FUNCTION ==========
    '==================================
    Public Function getFunction(ByVal name As String, ByVal nodeCaller As Node) As FunctionNode

        'Get file
        Dim currentFile As LimFile = getNodeParentFile(nodeCaller)

        'Search in Current file
        For Each currentFunction As FunctionNode In currentFile.functions
            If currentFunction.Name = name Then
                compileFunction(currentFunction)
                Return currentFunction
            End If
        Next

        'Search in others files
        For Each file As LimFile In currentFile.FilesImports
            For Each currentFunction As FunctionNode In file.functions
                If currentFunction.Name = name And currentFunction.export Then
                    compileFunction(currentFunction)
                    Return currentFunction
                End If
            Next
        Next

        'Anable to find the class
        addNodeNamingError("VBCGF01", "The function """ & name & """ could not be found.", nodeCaller, "Check the function name or that the file is correctly imported.")
        Return Nothing

    End Function

    '===================================
    '========== COMPILE CLASS ==========
    '===================================
    Private Sub compileClass(ByRef currentClass As ClassNode, Optional ByVal arguments As List(Of typeNode) = Nothing)

        'State handler
        If currentClass.compiled Then
            Exit Sub
        End If
        currentClass.compiled = True

        'Arguments
        If arguments Is Nothing Then
            arguments = New List(Of typeNode)
        End If

        'Get name
        If currentClass.compiledName = "" Then
            If getNodeParentFile(currentClass).LimLib Then
                currentClass.compiledName = currentClass.Name
            Else
                currentClass.compiledName = getClassName()
            End If
        End If

        'Fix name
        If getNodeParentFile(currentClass).LimLib And (currentClass.Name.StartsWith("__") And currentClass.Name.EndsWith("__")) Then
            currentClass.Name = currentClass.Name.Substring(2)
            currentClass.Name = currentClass.Name.Substring(0, currentClass.Name.Count - 2)
        End If

        'Some variables
        Dim content As New List(Of String)
        Dim new_content As New List(Of String)
        Dim new_function As FunctionNode

        'Searh new function
        For Each fun As FunctionNode In currentClass.methods
            If fun.Name = "new" Then
                new_function = fun
                Exit For
            End If
        Next

        'Header
        content.Add("")
        Dim headerText As String = "//////// " & currentClass.Name & " ////////"
        content.Add(StrDup(headerText.Length, "/"))
        content.Add(headerText)
        content.Add(StrDup(headerText.Length, "/"))

        'TODO: Fix str() & clone()

        'Define struct
        compiledStructsPrototypes.Add("struct " & currentClass.compiledName & ";")
        content.Add("struct " & currentClass.compiledName & "{")
        new_content.Add("struct " & currentClass.compiledName)

        'Declare variables
        For Each def As DeclareVariableNode In currentClass.declareVariables

            'Create variable
            Dim var As New Variable(def.variableName, Nothing, getVariableName(), def.declarationType)

            'Set variable type
            If def.variableUnsafeType Is Nothing Then

                'Check if brut value
                If Not ValueIsIndependent(def.value) Then
                    addNodeSyntaxError("VBCS01", "Only constants can be entered as the initialization value of a variable here.", def.value)
                End If

                'Set type
                var.type = getNodeType(def.value)

                'Compile
                content.Add(String.Format("{0} * {1};", compileTypeNode(var.type), var.compiledName))
                new_content.Add(String.Format("{0} = {1};", var.compiledName, compileNode(def.value, New List(Of String))))

            Else

                'Set type
                'var.type = typenodeToSafeType(def.variableUnsafeType)

                'Compile
                ' content.Add(String.Format("Public {0} As {1}", var.compiledName, compileSafeType(var.type)))

            End If

            'Add variable
            currentClass.variables.Add(var)

        Next

        'Finish struct
        content.Add("}")

        'Add content
        compiledClasss.AddRange(content)

    End Sub

    '======================================
    '========== COMPILE FUNCTION ==========
    '======================================
    Private Function compileFunction(ByRef fun As FunctionNode) As String

        'State handler
        If fun.compiled Then
            If fun.compiling Then
                addNodeSyntaxError("VBCCF01", "The type of this function must be explicitly noted by its use. Example: ""func " & fun.Name & "():str""", fun)
            End If
            Return ""
        End If
        fun.compiled = True
        fun.compiling = True

        'Struct
        Dim parentClass As ClassNode = Nothing
        If TypeOf fun.parentNode Is ClassNode Then
            parentClass = DirectCast(fun.parentNode, ClassNode)
        End If

        'Get unsafe type
        'If Not fun.unsafeReturnType Is Nothing Then
        '    fun.ReturnType = typenodeToSafeType(fun.unsafeReturnType)
        'End If

        'Some variables
        Dim content As New List(Of String)

        'Get name
        If fun.compiledName = "" Then

            'Normal handling
            If getNodeParentFile(fun).LimLib Then
                fun.compiledName = fun.Name
            Else
                fun.compiledName = getFunctionName()
            End If

        End If

        'Fix name
        If getNodeParentFile(fun).LimLib And (fun.Name.StartsWith("__") And fun.Name.EndsWith("__")) Then
            fun.Name = fun.Name.Substring(2)
            fun.Name = fun.Name.Substring(0, fun.Name.Count - 2)
        End If

        'Argument list
        Dim compiled_arguments As String = ""
        For Each arg As FunctionArgument In fun.Arguments

            Dim compiledName As String
            If getNodeParentFile(fun).LimLib Then
                compiledName = arg.name
            Else
                compiledName = getVariableName()
            End If

            'Dim var As New Variable(arg.name, typenodeToSafeType(arg.type), compiledName, arg.declareType)
            'fun.variables.Add(var)

            If arg.value IsNot Nothing Then

                'If Not ValueIsIndependent(arg.value) Then
                '    addNodeTypeError("VBCF01", "The value indicated as an optional parameter must be independent.", arg.value, "Put a default value, then, check in the function if the value of the argument is the default one. If yes, set the complex value.")
                'End If

                'compiled_arguments &= ", Optional " & var.compiledName & " As " & compileSafeType(var.type) & " = Nothing"
                'content.Add(String.Format("If {0} Is Nothing Then", var.compiledName))
                'content.Add(String.Format(" {0} = {1}", var.compiledName, compileNode(arg.value, content)))
                'content.Add("End If")

            Else

                'compiled_arguments &= ", " & var.compiledName & " As " & compileSafeType(var.type)

            End If

        Next
        If compiled_arguments.StartsWith(", ") Then
            compiled_arguments = compiled_arguments.Substring(2)
        End If

        'Compile content
        For Each action As Node In fun.codes

            Dim compiledAction As String = compileNode(action, content)
            If Not compiledAction = "" Then
                content.Add(compiledAction)
            End If

        Next



    End Function

    '======================================
    '========== COMPILE RELATION ==========
    '======================================
    Public Function compileRelation(ByVal relation As RelationNode) As String



    End Function

    '==================================
    '========== COMPILE TYPE ==========
    '==================================
    Private Function compileTypeNode(ByVal type As typeNode) As String

        'Get class
        If type.targetClass Is Nothing Then
            type.targetClass = getClass(type.className, type)
        End If

        'Compiled
        For Each compiledType As typeNode In compiledTypes

        Next

        'Variable
        Dim result As String

        'Compile

        'Return
        Return result

    End Function


    '===================================
    '========== GET NODE TYPE ==========
    '===================================
    Private Function getNodeType(ByVal node As Node) As Lim.typeNode

        Return Nothing

    End Function

    '==================================
    '========== COMPILE NODE ==========
    '==================================
    Private Function compileNode(ByVal node As Node, ByVal content As List(Of String)) As String

        Return Nothing


    End Function
End Class
