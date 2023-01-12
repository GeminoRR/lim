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

    Private compiledTypes As New List(Of Type)

    Private stdInt As Type
    Private stdFloat As Type
    Private stdStr As Type
    Private stdBool As Type

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
        stdInt = compileTypeNode(New typeNode(-1, -1, "int", New List(Of typeNode)))
        stdFloat = compileTypeNode(New typeNode(-1, -1, "float", New List(Of typeNode)))
        stdStr = compileTypeNode(New typeNode(-1, -1, "str", New List(Of typeNode)))
        stdBool = compileTypeNode(New typeNode(-1, -1, "bool", New List(Of typeNode)))

        'Logs
        If logs Then
            addLog("Load the ""std"" library: OK")
        End If

        'Compile start
        compileFunction(entryPoint, compiledFunctions)

        'Compile ASD functions
        For Each file As LimFile In files
            For Each fun As FunctionNode In file.functions
                If fun.AddSourceDirectly IsNot Nothing Then
                    compileFunction(fun, compiledFunctions)
                End If
            Next
        Next

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
                compileFunction(currentFunction, compiledFunctions)
                Return currentFunction
            End If
        Next

        'Search in others files
        For Each file As LimFile In currentFile.FilesImports
            For Each currentFunction As FunctionNode In file.functions
                If currentFunction.Name = name And currentFunction.export Then
                    compileFunction(currentFunction, compiledFunctions)
                    Return currentFunction
                End If
            Next
        Next

        'Anable to find the class
        addNodeNamingError("VBCGF01", "The function """ & name & """ could not be found.", nodeCaller, "Check the function name or that the file is correctly imported.")
        Return Nothing

    End Function

    '======================================
    '========== COMPILE FUNCTION ==========
    '======================================
    Private Sub compileFunction(ByRef fun As FunctionNode, ByVal targetResult As List(Of String), Optional ByVal context As context = Nothing)

        'State handler
        If fun.compiled Then
            If fun.compiling Then
                addNodeSyntaxError("VBCCF01", "The type of this function must be explicitly noted by its use. Example: ""func " & fun.Name & "():str""", fun)
            End If
            Exit Sub
        End If
        fun.compiled = True
        fun.compiling = True

        'Struct
        Dim parentType As Type = Nothing
        If context Is Nothing Then
            context = New context(fun)
        Else
            If TypeOf context.from Is Type Then
                parentType = DirectCast(context.from, Type)
                fun.unsafeReturnType = Nothing
                fun.ReturnType = parentType
            End If
        End If

        'Get unsafe type
        If Not fun.unsafeReturnType Is Nothing Then
            fun.ReturnType = compileTypeNode(fun.unsafeReturnType)
        End If

        'Get name
        If fun.compiledName = "" And fun.AddSourceDirectly Is Nothing Then

            'Normal handling
            If getNodeParentFile(fun).LimLib Then
                fun.compiledName = fun.Name
            Else
                fun.compiledName = getFunctionName()
            End If

        End If

        'Some variables
        Dim content As New List(Of String)
        Dim headerArguments As String = ""
        Dim compiledArguments As String = ""
        Dim optionnalArguments As New List(Of String)

        'Arguments
        For Each arg As FunctionArgument In fun.Arguments

            'Create var
            Dim var As New Variable(arg.name, Nothing, getVariableName(), arg.declareType)

            'Compile argument
            If arg.value IsNot Nothing Then

                'Is optionnal
                var.type = getNodeType(arg.value, context)

                'Compare to type
                If arg.type IsNot Nothing Then
                    Dim argType As Type = compileTypeNode(arg.type)
                    If Not argType.compiledName = var.type.compiledName Then
                        addNodeTypeError("CCCF01", "The type indicate (" & argType.ToString() & ") is not the same as that of the value (" & var.type.ToString() & ")", arg.type)
                    End If
                End If

                'Compile
                compiledArguments &= ", " & var.type.compiledName & " * " & var.compiledName
                optionnalArguments.Add("")
                optionnalArguments.Add("//Optionnal argument (" & arg.ToString() & ")")
                optionnalArguments.Add("if (" & var.compiledName & " == NULL){")
                Dim tempOptionnalArgument As New List(Of String)
                tempOptionnalArgument.Add(var.compiledName & " = " & compileNode(arg.value, tempOptionnalArgument, context) & ";")
                For Each line As String In tempOptionnalArgument
                    optionnalArguments.Add(vbTab & line)
                Next
                optionnalArguments.Add("}")

            Else

                'Has type
                var.type = compileTypeNode(arg.type)
                compiledArguments &= ", " & var.type.compiledName & " * " & var.compiledName

            End If

            'Add variable
            context.variables.Add(var)

            'Add to header
            headerArguments &= ", " & arg.ToString()

        Next
        If compiledArguments.StartsWith(", ") Then
            compiledArguments = compiledArguments.Substring(2)
        End If

        'Compile content
        Dim noneTabContent As New List(Of String)
        For Each action As Node In fun.codes

            Dim compiledAction As String = compileNode(action, noneTabContent, context)
            If Not compiledAction = "" Then
                noneTabContent.Add(compiledAction)
            End If

        Next

        'Define prototype
        Dim returnTypeSTR As String = "void"
        If fun.ReturnType IsNot Nothing Then
            returnTypeSTR = fun.ReturnType.compiledName
        End If

        compiledFunctionsPrototypes.Add(String.Format("{0} * {1}({2});", returnTypeSTR, fun.compiledName, compiledArguments))

        'Add header
        If headerArguments.StartsWith(", ") Then
            headerArguments = headerArguments.Substring(2)
        End If
        Dim headerReturnTypeSTR As String = ""
        If Not fun.ReturnType Is Nothing Then
            headerReturnTypeSTR = ":" & fun.ReturnType.ToString()
        End If
        content.Add("")
        Dim headerText As String = ""
        If parentType Is Nothing Then
            If fun.AddSourceDirectly Is Nothing Then
                headerText = "//////// FUNCTION: " & fun.Name & "(" & headerArguments & ")" & headerReturnTypeSTR & " ////////"
            Else
                headerText = "//////// ANONYMOUS FUNCTION" & headerReturnTypeSTR & " ////////"
            End If
        Else
            If fun.AddSourceDirectly Is Nothing Then
                headerText = "//////// " & parentType.Name & " MEHTOD: " & fun.Name & "(" & headerArguments & ")" & headerReturnTypeSTR & " ////////"
            Else
                headerText = "//////// ANONYMOUS METHOD" & headerReturnTypeSTR & " ////////"
            End If
        End If
        content.Add(StrDup(headerText.Length, "/"))
        content.Add(headerText)
        content.Add(StrDup(headerText.Length, "/"))

        'Define function
        If fun.AddSourceDirectly Is Nothing Then
            content.Add(String.Format("{0} * {1}({2})", returnTypeSTR, fun.compiledName, compiledArguments) & "{")
        Else
            content.Add(String.Format("{0} * {1}", returnTypeSTR, compileNode(fun.AddSourceDirectly, Nothing, context) & "{"))
        End If

        'Add arguments
        For Each line As String In optionnalArguments
            content.Add(vbTab & line)
        Next

        'Add compiled content
        content.Add(vbTab)
        For Each line As String In noneTabContent
            content.Add(vbTab & line)
        Next
        content.Add(vbTab)

        'End
        content.Add("}")

        'Result
        targetResult.AddRange(content)

    End Sub

    '======================================
    '========== COMPILE RELATION ==========
    '======================================
    Public Function compileRelation(ByVal relation As RelationNode) As String



    End Function

    '==================================
    '========== SEARCH CLASS ==========
    '==================================
    Private Function getClass(ByVal name As String, ByVal nodeCaller As Node, Optional ByVal throwError As Boolean = True) As ClassNode

        'Search current file
        Dim currentFile As LimFile = getNodeParentFile(nodeCaller, False)
        If currentFile Is Nothing Then
            currentFile = entryFile
        End If

        'Search in Current file
        For Each currentClass As ClassNode In currentFile.classs
            If currentClass.Name = name Then
                Return currentClass
            End If
        Next

        'Search in others files
        For Each otherFile As LimFile In currentFile.FilesImports
            For Each currentClass As ClassNode In otherFile.classs
                If currentClass.Name = name And currentClass.export Then
                    Return currentClass
                End If
            Next
        Next

        'Anable to find the class
        If throwError Then
            addNodeNamingError("CCGC01", "The class """ & name & """ could not be found.", nodeCaller, "Check the class name or that the file is correctly imported.")
        End If
        Return Nothing

    End Function

    '==================================
    '========== COMPILE TYPE ==========
    '==================================
    Private Function compileTypeNode(ByVal type As typeNode, Optional ByVal type_context As context = Nothing) As Type

        'Type already exist ?
        For Each compiled_type As Type In compiledTypes
            If compiled_type.Name = type.className Then

                'Pas asser d'argument
                If Not compiled_type.given_arguments.Count = type.arguments.Count Then
                    addNodeTypeError("CCCTN01", "The number of argument passed (" & type.arguments.Count.ToString() & ") does not correspond to the number requested by the class (" & compiled_type.given_arguments.Count.ToString() & ").", type)
                End If

                'Si les arguments match
                Dim match As Boolean = True
                For i As Integer = 0 To type.arguments.Count - 1

                    'Compile l'argument du typenode
                    Dim compiledArgument As Type = compileTypeNode(type.arguments(i))

                    'Vérifie si il est le même que celui given
                    If Not compiledArgument.compiledName = compiled_type.given_arguments(i).compiledName Then
                        match = False
                        Exit For
                    End If

                Next

                'Match
                If match Then
                    Return compiled_type
                End If

            End If
        Next

        'Search parent class argument (0 arguments because <parameter_name> are just simples strings)
        If type.arguments.Count = 0 Then
            Dim parentContext As context = type_context
            While parentContext IsNot Nothing

                'It's not a Type
                If Not TypeOf parentContext.from Is Type Then
                    parentContext = parentContext.upperContext
                    Continue While
                End If

                'It's a Type
                Dim castedType As Type = DirectCast(parentContext.from, Type)

                'Loop for each argument
                For i As Integer = 0 To castedType.arguments.Count - 1
                    If castedType.arguments(i) = type.className Then
                        Return castedType.given_arguments(i)
                    End If
                Next

            End While
        End If

        'We need to compile the type
        'Get class
        Dim target As ClassNode = getClass(type.className, type)

        'If match arguments count
        If Not target.arguments.Count = type.arguments.Count Then
            addNodeTypeError("CCCTN02", "The number of argument passed (" & type.arguments.Count.ToString() & ") does not correspond to the number requested by the class (" & target.arguments.Count.ToString() & ").", type)
        End If

        'Create new type
        Dim currentType As New Type(target.positionStart, target.positionEnd, target.Name, target.arguments)
        currentType.parentNode = target.parentNode
        currentType.compiled = False

        'Create context
        Dim context As New context(currentType)

        'Get parent file
        Dim parentFile As LimFile = getNodeParentFile(currentType)

        'Get name
        If parentFile.LimLib Then

            'LimLib
            Dim dimensions_str As String = ""
            If type.arguments.Count > 0 Then
                For Each arg As typeNode In type.arguments
                    dimensions_str &= "_and_" & compileTypeNode(arg).compiledName
                Next
                dimensions_str = "_of_" & dimensions_str.Substring(5) & "_end"
            End If

            'Set name
            If Not target.compiledName = "" Then

                'Compiled name already exist
                currentType.compiledName = target.compiledName & dimensions_str

            Else

                'Use the name
                currentType.compiledName = target.Name & dimensions_str

            End If

        Else

            'Generate a name
            currentType.compiledName = getClassName()

        End If

        'Add to list of compiled types
        compiledTypes.Add(currentType)

        'Compile arguments
        For Each arg As typeNode In type.arguments
            currentType.given_arguments.Add(compileTypeNode(arg))
        Next

        'Copy methods
        Dim new_method As FunctionNode = Nothing
        Dim str_method As FunctionNode = Nothing
        Dim clone_method As FunctionNode = Nothing
        For Each method As FunctionNode In target.methods

            'Clone
            currentType.methods.Add(method)

            'Search methods
            If method.Name = "new" Then
                new_method = method
            ElseIf method.Name = "str" Then
                str_method = method
            ElseIf method.Name = "clone" Then
                clone_method = method
            End If

        Next

        'Copy relations
        currentType.relations = target.relations

        'Some variables
        Dim content As New List(Of String)
        Dim new_content As New List(Of String)

        'Header
        content.Add("")
        Dim headerArguments As String = ""
        For Each arg As Type In currentType.given_arguments
            headerArguments &= ", " & arg.ToString()
        Next
        If headerArguments.StartsWith(", ") Then
            headerArguments = "<" & headerArguments.Substring(2) & ">"
        End If
        Dim headerText As String = "//////// CLASS: " & currentType.Name & headerArguments & " ////////"
        content.Add(StrDup(headerText.Length, "/"))
        content.Add(headerText)
        content.Add(StrDup(headerText.Length, "/"))

        'Define struct
        compiledStructsPrototypes.Add("struct " & currentType.compiledName & ";")
        content.Add("struct " & currentType.compiledName & "{")

        'AddSourceDirectly
        For Each ASD As AddSourceNode In target.addSourceDirectly

            content.Add(vbTab & ASD.value)

        Next

        'Declare variables
        For Each def As DeclareVariableNode In target.declareVariables

            'Create variable
            Dim var As New Variable(def.variableName, Nothing, getVariableName(), def.declarationType)

            'Set variable type
            If def.variableUnsafeType Is Nothing Then

                'Check if brut value
                If Not ValueIsIndependent(def.value) Then
                    addNodeSyntaxError("CCCT03", "Only constants can be entered as the initialization value of a variable in a class.", def.value)
                End If

                'Set type
                var.type = getNodeType(def.value, context)

                'Compile
                content.Add(vbTab & String.Format("struct {0} * {1};", var.type.compiledName, var.compiledName))
                new_content.Add(vbTab & String.Format("/* {0} */ {1} = {2};", var.name, var.compiledName, compileNode(def.value, New List(Of String), context)))

            Else

                'Set type
                var.type = compileTypeNode(def.variableUnsafeType)

                'Compile
                content.Add(vbTab & String.Format("struct {0} * {1};", var.type.compiledName, var.compiledName))

            End If

            'Add variable
            context.variables.Add(var)

        Next

        'Finish struct
        content.Add("}")

        'Pre-New content
        Dim preprocess As New List(Of String)
        preprocess.Add("")
        preprocess.Add(vbTab & "//Allocate memory")
        preprocess.Add(vbTab & String.Format("struct {0} * self = tgc_alloc(&gc, sizeof(struct {0}));", currentType.compiledName))
        preprocess.Add(vbTab)
        If new_content.Count > 0 Then
            preprocess.Add(vbTab & "//Initialize property values")
            preprocess.AddRange(new_content)
            preprocess.Add(vbTab)
        End If

        'New
        Dim new_method_content As New List(Of String)
        new_method.compiledName = currentType.compiledName & "_new"
        compileFunction(new_method, new_method_content, context)
        new_method_content.InsertRange(5, preprocess)
        new_method_content.Insert(new_method_content.Count - 1, vbTab & "//Return object")
        new_method_content.Insert(new_method_content.Count - 1, vbTab & "return self;")
        new_method_content.Insert(new_method_content.Count - 1, vbTab)
        content.AddRange(new_method_content)

        'Add content
        compiledClasss.AddRange(content)

        'Compile methods
        For Each method As FunctionNode In currentType.methods
            compileFunction(method, content, context)
        Next

        'TODO: Compile relations

        'Compiled
        currentType.compiled = True

        'Return
        Return currentType

    End Function

    '===================================
    '========== COMPILE VALUE ==========
    '===================================
    Public Function compileValue(ByVal node As valueNode, ByVal content As List(Of String), ByVal context As context) As String

        'Return
        Select Case node.tok.type

            Case tokenType.CT_FLOAT
                Return "new_float(" & node.tok.value.ToString() & ")"

            Case tokenType.CT_INTEGER
                Return "new_int(" & node.tok.value.ToString() & ")"

            Case Else
                Throw New NotImplementedException

        End Select

    End Function

    '========================================
    '========== COMPILE ADD SOURCE ==========
    '========================================
    Private Function compileAddSource(ByVal node As AddSourceNode, ByVal content As List(Of String), ByVal context As context)

        'Check if file is limlib
        If getNodeParentFile(node).LimLib Then

            'Compile
            Return DirectCast(node, AddSourceNode).value

        Else

            'Error
            addNodeSyntaxError("VBCCN02", "It Is Not possible To integrate source code outside Of a ""limlib"" file.", node)

        End If

        'Return
        Return ""

    End Function

    '==============================================
    '========== COMPILE DECLARE VARIABLE ==========
    '==============================================
    Private Function compileDeclareVariable(ByVal node As DeclareVariableNode, ByVal content As List(Of String), ByVal context As context) As String

        'Declare variable
        content.Add("")
        content.Add(String.Format("//{0}", node.variableName))

        'Set variable
        Dim compiledVariableName As String
        If getNodeParentFile(node).LimLib Then
            compiledVariableName = node.variableName
        Else
            compiledVariableName = getVariableName()
        End If
        Dim var As New Variable(node.variableName, Nothing, compiledVariableName, node.declarationType)

        'Get type
        If node.value Is Nothing Then

            'Set type
            var.type = compileTypeNode(node.variableUnsafeType)

            'Compile
            content.Add(String.Format("{0} * {1} = NULL;", var.type.compiledName, var.compiledName))

        Else

            'Set type
            var.type = getNodeType(node.value, context)
            If node.variableUnsafeType IsNot Nothing Then
                Dim defType As Type = compileTypeNode(node.variableUnsafeType)
                If Not var.type = defType Then
                    addNodeTypeError("VBCCF02", "The defined type of the variable (" & defType.ToString() & ") is not the same as that of its value (" & var.type.ToString() & ").", node.value)
                End If
            End If

            'Ref
            Select Case var.declarationType

                Case VariableDeclarationType._let_
                    content.Add(String.Format("{0} * {1} = {2};", var.type.compiledName, var.compiledName, compileNode(node.value, content, context)))

                Case VariableDeclarationType._var_
                    content.Add(String.Format("{0} * {1} = {0}_clone({2});", var.type.compiledName, var.compiledName, compileNode(node.value, content, context)))

                Case Else
                    Throw New NotImplementedException

            End Select

        End If

        'Add variable
        context.variables.Add(var)

        'Return
        content.Add("")
        Return ""

    End Function

    '==========================================
    '========== COMPILE SET VARIABLE ==========
    '==========================================
    Private Function compileSetVariable(ByVal node As SetVariableNode, ByVal content As List(Of String), ByVal context As context) As String

        'Set variable
        Dim castedNode As SetVariableNode = DirectCast(node, SetVariableNode)

        'Get type
        Dim variableType As Type = getNodeType(castedNode.Target, context)
        Dim valueType As Type = getNodeType(castedNode.NewValue, context)

        'Handle error
        If variableType = valueType Then
            addNodeTypeError("VBCSV01", "It is impossible for a variable of type <" & variableType.ToString() & "> to assign itself a value of type <" & valueType.ToString() & ">", castedNode.NewValue)
        End If

        'Get variable
        If TypeOf castedNode.Target Is VariableNode Then

            'Get variable
            Dim var As Variable = Nothing
            var = getVariable(DirectCast(castedNode.Target, VariableNode).VariableName, castedNode.Target)

            'Compile
            Select Case var.declarationType

                Case VariableDeclarationType._let_
                    content.Add(String.Format("{0} = {1};", var.compiledName, compileNode(castedNode.NewValue, content, context)))

                Case VariableDeclarationType._var_
                    content.Add(String.Format("{0} = {1}_clone({2});", var.compiledName, var.type.compiledName, compileNode(castedNode.NewValue, content, context)))

                Case Else
                    Throw New NotImplementedException

            End Select

        ElseIf TypeOf castedNode.Target Is BracketsSelectorNode Then

            'Compile
            'TODO: DO
            content.Add(String.Format("{0} = {1};", compileNode(castedNode.Target, content, context), compileNode(castedNode.NewValue, content, context)))


        ElseIf TypeOf castedNode.Target Is childNode Then

            'Compile
            'TODO: DO
            content.Add(String.Format("{0}->{1} = {1};", compileNode(castedNode.Target, content, context), compileNode(castedNode.NewValue, content, context)))


        Else
            addNodeSyntaxError("VBCSV02", "It is not possible to assign a value to this target", castedNode.Target)

        End If

        'Return
        Return ""

    End Function

    '==================================
    '========== COMPILE NODE ==========
    '==================================
    Private Function compileNode(ByVal node As Node, ByVal content As List(Of String), ByVal context As context) As String

        If TypeOf node Is DeclareVariableNode Then

            'Compile
            Return compileDeclareVariable(DirectCast(node, DeclareVariableNode), content, context)

        ElseIf TypeOf node Is SetVariableNode Then

            'Compîle
            Return compileSetVariable(DirectCast(node, SetVariableNode), content, context)

        ElseIf TypeOf node Is valueNode Then

            'Compile
            Return compileValue(DirectCast(node, valueNode), content, context)

        ElseIf TypeOf node Is AddSourceNode Then

            'Compile
            Return compileAddSource(DirectCast(node, AddSourceNode), content, context)

        End If

        'Return
        addNodeTypeError("VBCCN01", "Unable to resolve node type", node)
        Return Nothing

    End Function

    '===================================
    '========== GET NODE TYPE ==========
    '===================================
    Private Function getNodeType(ByVal node As Node, ByVal context As context) As Type

        'Unsafe type
        If TypeOf node Is typeNode Then

            'Return
            Return compileTypeNode(DirectCast(node, typeNode))

        End If

        'valueNode
        If TypeOf node Is valueNode Then

            'Casted Node
            Dim castedNode As valueNode = DirectCast(node, valueNode)

            'Return
            Select Case castedNode.tok.type
                Case tokenType.CT_INTEGER
                    Return stdInt

                Case tokenType.CT_FLOAT
                    Return stdFloat

                Case Else
                    addBasicError("Type error", "Cannot get type of the folowing token <" & castedNode.tok.ToString() & ">")
                    Return Nothing

            End Select

        End If

        'Return
        addNodeTypeError("CCGNT01", "Unable to resolve node type", node)
        Return Nothing

    End Function


End Class
