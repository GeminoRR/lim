Imports System.IO
Public Class C_Compiler

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public files As New List(Of LimFile)
    Private entryFile As LimFile

    Private compiledImports As New List(Of String)
    Private compiledStructsPrototypes As New List(Of String)
    Private compiledTypeDefs As New List(Of String)
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
    Private stdList As Type

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
        compiledTypeDefs.Clear()
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

        'TypeDef
        finalCode &= "//////////////////////////" & Environment.NewLine
        finalCode &= "//////// TYPE DEF ////////" & Environment.NewLine
        finalCode &= "//////////////////////////"
        For Each line As String In compiledTypeDefs
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

        'Add TGC
        File.Copy(cTemplates & "/gc/tgc.c", outputFolder & "/tgc.c", True)
        File.Copy(cTemplates & "/gc/tgc.h", outputFolder & "/tgc.h", True)

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
    '========== GET CONTEXTS ==========
    '==================================
    Private Iterator Function getContexts(ByVal context As context) As System.Collections.IEnumerable

        Dim upperContext As context = context

        While upperContext IsNot Nothing

            Yield upperContext
            upperContext = upperContext.upperContext

        End While

    End Function

    '==================================
    '========== GET VARIABLE ==========
    '==================================
    Public Function getVariable(ByVal name As String, ByVal context As context, Optional ByVal nodeCaller As Node = Nothing) As Tuple(Of Variable, Type)

        'Search in current context
        For Each ctx As context In getContexts(context)

            For Each var As Variable In ctx.variables
                If var.name = name Then
                    Dim type_associeted_with_variable As Type = Nothing
                    If TypeOf ctx.from Is Type Then
                        type_associeted_with_variable = ctx.from
                    End If
                    Return New Tuple(Of Variable, Type)(var, type_associeted_with_variable)
                End If
            Next

        Next

        'Get file
        Dim currentFile As LimFile = getNodeParentFile(context.from)

        'Search in current file variables
        For Each var As Variable In currentFile.variables
            If var.name = name Then
                Return New Tuple(Of Variable, Type)(var, Nothing)
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
                    Return New Tuple(Of Variable, Type)(var, Nothing)
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
        If nodeCaller Is Nothing Then
            nodeCaller = context.from
        End If
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
        addNodeNamingError("CCCGF01", "The function """ & name & """ could not be found.", nodeCaller, "Check the function name or that the file is correctly imported.")
        Return Nothing

    End Function

    '======================================
    '========== COMPILE FUNCTION ==========
    '======================================
    Private Sub compileFunction(ByRef fun As FunctionNode, ByVal targetResult As List(Of String), Optional ByVal up_context As context = Nothing)

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
        Dim context As context = New context(fun, up_context)
        If up_context IsNot Nothing Then
            If TypeOf up_context.from Is Type Then
                parentType = DirectCast(up_context.from, Type)
            End If
        End If

        'Get name
        Dim isLimLib As Boolean = getNodeParentFile(fun).LimLib
        If fun.compiledName = "" And fun.AddSourceDirectly Is Nothing Then

            'Normal handling
            If isLimLib Then
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

        'Fix name with class & add self
        If parentType IsNot Nothing Then
            If Not fun.compiledName = "new" Then
                compiledArguments = parentType.compiledName & "* self"
                optionnalArguments.Add("")
                optionnalArguments.Add("//Object NULL")
                optionnalArguments.Add("if (self == NULL){")
                optionnalArguments.Add(vbTab & "fatalError(""The \""" & fun.Name & "\"" method of the <" & parentType.Name & "> class was used on a \""null\"" variable. No object has been instantiated."");")
                optionnalArguments.Add("}")
                optionnalArguments.Add("")
            End If
            fun.compiledName = parentType.compiledName & "_" & fun.compiledName
        End If

        'Get unsafe type
        If fun.unsafeReturnType IsNot Nothing Then
            fun.ReturnType = compileTypeNode(fun.unsafeReturnType, context)
        End If

        'Arguments
        For Each arg As FunctionArgument In fun.Arguments

            'Create var
            Dim variablename As String = ""
            If isLimLib Then
                variablename = arg.name
            Else
                variablename = getVariableName()
            End If
            Dim var As New Variable(arg.name, Nothing, variablename, arg.doubleRef)

            'Compile argument
            If arg.value IsNot Nothing Then

                'Is optionnal

                'Check type (independent)
                If Not ValueIsIndependent(arg.value) Then
                    addNodeTypeError("CCCF01", "Only a constant value can be put in parameter.", arg.value, "Indicate a basic ""null"" value, then in the function, following a condition, indicate the value you want.")
                End If
                var.type = getNodeType(arg.value, context)

                'Compare to type
                If arg.type IsNot Nothing Then
                    Dim argType As Type = compileTypeNode(arg.type, context)
                    If Not argType.compiledName = var.type.compiledName Then
                        addNodeTypeError("CCCF02", "The type indicate (" & argType.ToString() & ") is not the same as that of the value (" & var.type.ToString() & ")", arg.type)
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
                var.type = compileTypeNode(arg.type, context)
                If arg.doubleRef Then
                    If var.type.primary Then
                        addNodeTypeError("CCCF03", "A double-refer argument cannot take a primary type value.", arg.type)
                    End If
                    compiledArguments &= ", " & var.type.compiledName & " ** " & var.compiledName
                Else
                    compiledArguments &= ", " & var.type.compiledName & " * " & var.compiledName
                End If

            End If

            'Add variable
            context.variables.Add(var)

            'Add type
            arg.compiledType = var.type

            'Add to header
            headerArguments &= ", " & arg.ToString()

        Next
        If compiledArguments.StartsWith(", ") Then
            compiledArguments = compiledArguments.Substring(2)
        End If

        'Compile content
        Dim noneTabContent As New List(Of String)
        For Each action As Node In fun.content

            Dim compiledAction As String = compileNode(action, noneTabContent, context)
            If Not compiledAction = "" Then
                noneTabContent.Add(compiledAction)
            End If

        Next

        'Standard methods
        If parentType IsNot Nothing Then

            Select Case fun.Name

                Case "clone"
                    'Empty Clone method
                    If fun.content.Count = 0 Then
                        noneTabContent.Add("")
                        noneTabContent.Add("//Create new object")
                        noneTabContent.Add(parentType.compiledName & " * clone = tgc_alloc(&gc, sizeof(" & parentType.compiledName & "));")
                        noneTabContent.Add("")
                        noneTabContent.Add("//Copy properties")
                        For Each propertie As Variable In parentType.variables
                            noneTabContent.Add("/* " & propertie.name & " */ clone->" & propertie.compiledName & " = " & propertie.type.compiledName & "_clone(self->" & propertie.compiledName & ");")
                        Next
                        noneTabContent.Add("")
                        noneTabContent.Add("//Return")
                        noneTabContent.Add("return clone;")
                        Exit Select

                    End If

                    'Return type error
                    If fun.ReturnType Is Nothing Then
                        addNodeTypeError("CCCF03", "The clone method need to return an object of its own type.", fun)
                    End If
                    If Not fun.ReturnType = parentType Then
                        addNodeTypeError("CCCF04", "The clone method can only return an object of its own type.", fun)
                    End If

                Case "str"
                    'Empty Str method
                    If fun.content.Count = 0 Then
                        noneTabContent.Add("")
                        noneTabContent.Add("//Create string")
                        noneTabContent.Add("char * temp = tgc_alloc(&gc, sizeof(char) * (" & parentType.Name.Length.ToString() & " + 21));")
                        noneTabContent.Add("sprintf(temp, ""<" & parentType.Name.ToString() & ", %p>"", self);")
                        noneTabContent.Add("")
                        noneTabContent.Add("//Return")
                        noneTabContent.Add("return new_str(temp);")
                        Exit Select

                    End If

                    'Return type error
                    If fun.ReturnType Is Nothing Then
                        addNodeTypeError("CCCF05", "The str method need to return a string (<str>).", fun)
                    End If
                    If Not fun.ReturnType.compiledName = "__str__" Then
                        addNodeTypeError("CCCF06", "The str method can only return a string (<str>).", fun)
                    End If

                Case "repr"
                    'Empty Repr method
                    If fun.content.Count = 0 Then
                        noneTabContent.Add("//Return")
                        noneTabContent.Add("return " & parentType.compiledName & "_str(self);")
                        Exit Select

                    End If

                    'Return type error
                    If fun.ReturnType Is Nothing Then
                        addNodeTypeError("CCCF07", "The repr method need to return a string (<str>).", fun)
                    End If
                    If Not fun.ReturnType.compiledName = "__str__" Then
                        addNodeTypeError("CCCF08", "The repr method can only return a string (<str>).", fun)
                    End If

            End Select

        End If

        'Define prototype
        Dim returnTypeSTR As String = "void"
        If fun.ReturnType IsNot Nothing Then
            returnTypeSTR = fun.ReturnType.compiledName
        End If

        If fun.AddSourceDirectly Is Nothing Then
            compiledFunctionsPrototypes.Add(String.Format("{0} * {1}({2});", returnTypeSTR, fun.compiledName, compiledArguments))
        Else
            compiledFunctionsPrototypes.Add(String.Format("{0} * {1};", returnTypeSTR, compileNode(fun.AddSourceDirectly, Nothing, context)))
        End If

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
            content.Add(StrDup(headerText.Length, "/"))
            content.Add(headerText)
            content.Add(StrDup(headerText.Length, "/"))
        Else
            If fun.AddSourceDirectly Is Nothing Then
                headerText = "//////// " & parentType.Name & " MEHTOD: " & fun.Name & "(" & headerArguments & ")" & headerReturnTypeSTR & " ////////"
            Else
                headerText = "//////// ANONYMOUS METHOD" & headerReturnTypeSTR & " ////////"
            End If
            content.Add(headerText)
        End If

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
        For Each line As String In noneTabContent
            content.Add(vbTab & line)
        Next
        content.Add(vbTab)

        'End
        content.Add("}")

        'Result
        fun.compiling = False
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

        If type.className = "item_type" And type_context IsNot Nothing Then
            If type_context.upperContext Is Nothing Then
                Debugger.Break()
                Return Nothing
            End If
        End If
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
                    Dim compiledArgument As Type = compileTypeNode(type.arguments(i), type_context)

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

                'Go upper
                parentContext = parentContext.upperContext

            End While
        End If

        'We need to compile the type"
        'Get class
        Dim target As ClassNode = getClass(type.className, type)

        'If match arguments count
        If Not target.arguments.Count = type.arguments.Count Then
            addNodeTypeError("CCCTN02", "The number of argument passed (" & type.arguments.Count.ToString() & ") does not correspond to the number requested by the class (" & target.arguments.Count.ToString() & ").", type)
        End If

        'Create new type
        Dim currentType As New Type(target.positionStart, target.positionEnd, target.Name, target.arguments)
        currentType.parentNode = target.parentNode
        currentType.primary = target.primary
        currentType.compiled = False

        'Create context
        Dim context As New context(currentType, type_context)

        'Get parent file
        Dim parentFile As LimFile = getNodeParentFile(currentType)

        'Get name
        If parentFile.LimLib Then

            'LimLib
            Dim dimensions_str As String = ""
            If type.arguments.Count > 0 Then
                For Each arg As typeNode In type.arguments
                    dimensions_str &= "_and_" & compileTypeNode(arg, context).compiledName
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
            currentType.given_arguments.Add(compileTypeNode(arg, context))
        Next

        'Copy methods
        Dim new_method As FunctionNode = Nothing
        Dim str_method As FunctionNode = Nothing
        Dim clone_method As FunctionNode = Nothing
        Dim repr_method As FunctionNode = Nothing
        For Each method As FunctionNode In target.methods

            'Clone
            method = method.clone()
            currentType.methods.Add(method)

            'Search methods
            If method.Name = "new" Then
                new_method = method
                new_method.compiledName = "new"
                If new_method.unsafeReturnType Is Nothing Then
                    new_method.ReturnType = currentType
                End If
            ElseIf method.Name = "str" Then
                str_method = method
                str_method.compiledName = "str"
                If str_method.unsafeReturnType Is Nothing Then
                    str_method.ReturnType = stdStr
                End If
            ElseIf method.Name = "repr" Then
                repr_method = method
                repr_method.compiledName = "repr"
                If repr_method.unsafeReturnType Is Nothing Then
                    repr_method.ReturnType = stdStr
                End If
            ElseIf method.Name = "clone" Then
                clone_method = method
                clone_method.compiledName = "clone"
                If clone_method.unsafeReturnType Is Nothing Then
                    clone_method.ReturnType = currentType
                End If
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
        compiledTypeDefs.Add("typedef struct " & currentType.compiledName & " " & currentType.compiledName & ";")
        content.Add("struct " & currentType.compiledName & "{")

        'AddSourceDirectly
        For Each ASD As AddSourceNode In target.addSourceDirectly

            ASD.allLine = False
            content.Add(vbTab & compileAddSource(ASD, Nothing, context))

        Next

        'Declare variables
        For Each def As DeclareVariableNode In target.declareVariables

            'Create variable
            Dim var As New Variable(def.variableName, Nothing, getVariableName())

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
                new_content.Add(vbTab & String.Format("/* {0} */ self->{1} = {2};", var.name, var.compiledName, compileNode(def.value, New List(Of String), context)))

            Else

                'Set type
                var.type = compileTypeNode(def.variableUnsafeType, context)

                'Compile
                content.Add(vbTab & String.Format("struct {0} * {1};", var.type.compiledName, var.compiledName))

            End If

            'Add variable
            currentType.variables.Add(var)
            context.variables.Add(var)

        Next

        'Finish struct
        content.Add("};")

        'Pre-New content
        Dim preprocess As New List(Of String)
        preprocess.Add("")
        preprocess.Add(vbTab & "//Allocate memory")
        preprocess.Add(vbTab & String.Format("{0} * self = tgc_alloc(&gc, sizeof({0}));", currentType.compiledName))
        If new_content.Count > 0 Then
            preprocess.Add(vbTab)
            preprocess.Add(vbTab & "//Initialize property values")
            preprocess.AddRange(new_content)
        End If

        'New
        Dim new_method_content As New List(Of String)
        compileFunction(new_method, new_method_content, context)
        new_method_content.InsertRange(3, preprocess)
        new_method_content.Insert(new_method_content.Count - 1, vbTab & "//Return object")
        new_method_content.Insert(new_method_content.Count - 1, vbTab & "return self;")
        new_method_content.Insert(new_method_content.Count - 1, vbTab)
        content.AddRange(new_method_content)

        'Compile methods
        For Each method As FunctionNode In currentType.methods
            compileFunction(method, content, context)
        Next

        'TODO: Compile relations

        'Add content
        compiledClasss.AddRange(content)

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

            'Search for variables
            Dim result As String = node.value
            If result.Contains("{{") And result.Contains("}}") Then

                'Types
                Dim parentContext As context = context
                While parentContext IsNot Nothing

                    'Variables
                    For Each var As Variable In parentContext.variables
                        result = result.Replace("{{" & var.name & "}}", var.compiledName)
                    Next

                    'It's not a Type
                    If Not TypeOf parentContext.from Is Type Then
                        parentContext = parentContext.upperContext
                        Continue While
                    End If

                    'It's a Type
                    Dim castedType As Type = DirectCast(parentContext.from, Type)

                    'Type
                    result = result.Replace("{{self}}", castedType.compiledName)

                    'Loop for each argument
                    For i As Integer = 0 To castedType.arguments.Count - 1
                        result = result.Replace("{{" & castedType.arguments(i) & "}}", castedType.given_arguments(i).compiledName)
                    Next

                    'Go upper
                    parentContext = parentContext.upperContext

                End While


            End If

            'Compile
            If node.allLine Then
                content.Add(result)
            Else
                Return result
            End If

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
        content.Add(String.Format("//Create ""{0}"" variable", node.variableName))

        'Set variable
        Dim compiledVariableName As String
        If getNodeParentFile(node).LimLib Then
            compiledVariableName = node.variableName
        Else
            compiledVariableName = getVariableName()
        End If
        Dim var As New Variable(node.variableName, Nothing, compiledVariableName)

        'Get type
        If node.value Is Nothing Then

            'Set type
            var.type = compileTypeNode(node.variableUnsafeType, context)

            'Compile
            content.Add(String.Format("{0} * {1} = NULL;", var.type.compiledName, var.compiledName))

        Else

            'Set type
            var.type = getNodeType(node.value, context)
            If node.variableUnsafeType IsNot Nothing Then
                Dim defType As Type = compileTypeNode(node.variableUnsafeType, context)
                If Not var.type = defType Then
                    addNodeTypeError("VBCCF02", "The defined type of the variable (" & defType.ToString() & ") is not the same as that of its value (" & var.type.ToString() & ").", node.value)
                End If
            End If

            'Compile
            content.Add(String.Format("{0} * {1} = {2};", var.type.compiledName, var.compiledName, compileNode(node.value, content, context)))

        End If

        'Add variable
        context.variables.Add(var)

        'Return
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
        If Not variableType = valueType Then
            addNodeTypeError("CCCSV01", "It is impossible for a variable of type <" & variableType.ToString() & "> to assign itself a value of type <" & valueType.ToString() & ">", castedNode.NewValue)
        End If

        'Content
        content.Add("")
        content.Add("//Set variable")

        'Get variable
        If TypeOf castedNode.Target Is VariableNode Then

            'Get variable
            Dim castedTarget As VariableNode = DirectCast(castedNode.Target, VariableNode)
            Dim varResult As Tuple(Of Variable, Type) = getVariable(castedTarget.VariableName, context, castedTarget)
            Dim var As Variable = varResult.Item1

            'Compile
            content.Add(String.Format("{0} = {1};", compileVariable(castedTarget, content, context, True), compileNode(castedNode.NewValue, content, context)))

        ElseIf TypeOf castedNode.Target Is BracketsSelectorNode Then

            'Compile
            'TODO: DO
            content.Add(String.Format("{0} = {1};", compileNode(castedNode.Target, content, context), compileNode(castedNode.NewValue, content, context)))


        ElseIf TypeOf castedNode.Target Is childNode Then

            'Compile
            content.Add(String.Format("{0} = {1};", compileChild(castedNode.Target, content, context, True), compileNode(castedNode.NewValue, content, context)))


        Else
            addNodeSyntaxError("CCCSV02", "It is not possible to assign a value to this target", castedNode.Target)

        End If

        'Return
        Return ""

    End Function

    '=====================================
    '========== COMPILE BOOLEAN ==========
    '=====================================
    Public Function compileBoolean(ByVal node As BooleanNode) As String

        'Return
        If node.value Then
            Return "new_bool(true)"
        Else
            Return "new_bool(false)"
        End If

    End Function

    '====================================
    '========== COMPILE STRING ==========
    '====================================
    Public Function compileString(ByVal node As StringNode) As String

        'Return
        Return "new_str(""" & node.value.ToString().Replace("""", "\""") & """)"

    End Function

    '=============================================
    '========== COMPILE WHILE STATEMENT ==========
    '=============================================
    Private Function compileWhileStatement(ByVal node As whileStatementNode, ByVal content As List(Of String), ByVal context As context)

        'Check type
        Dim conditionType As Type = getNodeType(node.condition, context)
        If Not conditionType = stdBool Then
            addNodeTypeError("CCCWS01", "The condition of a while loop must be of type <bool>. However, you indicate a value of type <" & conditionType.ToString() & ">", node.condition)
        End If

        'Compile while header
        content.Add("")
        content.Add("while (" & compileNode(node.condition, content, context) & "->value){")

        'Compile core
        Dim core As New List(Of String)
        For Each line As Node In node.content
            core.Add(compileNode(line, core, context))
        Next
        For Each line As String In core
            content.Add(vbTab & line)
        Next

        'End
        content.Add("}")
        content.Add("")

        'Return
        Return ""

    End Function

    '===========================================
    '========== COMPILE FUNCTION CALL ==========
    '===========================================
    Private Function compileFunctionCall(ByVal node As FunctionCallNode, ByVal content As List(Of String), ByVal context As context) As String

        'Get function
        Dim fun As FunctionNode = getFunction(node.FunctionName, node)

        'Handle argument error
        If node.Arguments.Count < fun.minArguments Then
            addNodeTypeError("CCCFC01", (fun.Arguments.Count - node.Arguments.Count).ToString() & " arguments are missing", node)
        End If
        If node.Arguments.Count > fun.maxArguments Then
            addNodeTypeError("CCCFC02", (node.Arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
        End If

        'Argument
        Dim arguments As String = ""
        For i As Integer = 0 To fun.Arguments.Count - 1

            'Fill optionnal argument
            If i >= node.Arguments.Count Then
                arguments &= ", NULL"
                Continue For
            End If

            'Variables
            Dim argumentModel As FunctionArgument = fun.Arguments(i)
            Dim argumentValue As Node = node.Arguments(i)

            'Handle type error
            If Not (argumentModel.compiledType = getNodeType(argumentValue, context)) Then
                addNodeTypeError("CCCFC03", "The " & (i + 1).ToString() & " argument is of type <" & getNodeType(argumentValue, context).ToString() & "> instead of being <" & argumentModel.type.ToString() & ">", argumentValue)
            End If

            'Add argument
            If argumentModel.doubleRef Then
                If (TypeOf argumentValue IsNot VariableNode) Then
                    addNodeTypeError("CCCFC04", "This argument can only take variables as its value.", argumentValue)
                End If
                arguments &= ", &(" & compileNode(argumentValue, content, context) & ")"
            Else
                arguments &= ", " & compileNode(argumentValue, content, context)
            End If

        Next
        If arguments.StartsWith(", ") Then
            arguments = arguments.Substring(2)
        End If

        'Return
        If node.allLineFunction Then
            content.Add("")
            content.Add("//Function call to """ & fun.Name & """")
            content.Add(fun.compiledName & "(" & arguments & ");")
            Return ""
        Else
            Return fun.compiledName & "(" & arguments & ")"
        End If

    End Function

    '======================================
    '========== COMPILE VARIABLE ==========
    '======================================
    Public Function compileVariable(ByVal node As VariableNode, ByVal content As List(Of String), ByVal context As context, Optional force_reference As Boolean = False) As String

        'Get variable
        Dim varResult As Tuple(Of Variable, Type) = getVariable(node.VariableName, context, node)
        Dim var As Variable = varResult.Item1

        'Is a properties ?
        Dim result As String = var.compiledName
        If varResult.Item2 IsNot Nothing Then
            result = "self->" & result
        End If

        'DoubleReference
        If var.doubleRef Then
            result = "(*(" & result & "))"
        End If

        'Clone
        If var.type.primary And force_reference = False Then
            Return var.type.compiledName & "_clone(" & result & ")"
        Else
            Return result
        End If

    End Function

    '===================================
    '========== COMPILE CHILD ==========
    '===================================
    Private Function compileChild(ByVal node As childNode, ByVal content As List(Of String), ByVal context As context, Optional ByVal force_reference As Boolean = False)

        'Get class
        Dim parentType As Type = getNodeType(node.parentStruct, context)

        'Compile
        If TypeOf node.childNode Is VariableNode Then

            'Variables
            Dim searchName As String = DirectCast(node.childNode, VariableNode).VariableName

            'Search propertie
            For Each var As Variable In parentType.variables
                If var.name = searchName Then

                    'Compile result
                    Dim result As String
                    If var.type.primary And force_reference = False Then
                        result = String.Format("{2}_clone({0}->{1})", compileNode(node.parentStruct, content, context), var.compiledName, var.type.compiledName)
                    Else
                        result = String.Format("{0}->{1}", compileNode(node.parentStruct, content, context), var.compiledName)
                    End If

                    'Return
                    If node.allLine Then
                        content.Add(result & ";")
                        Return ""
                    Else
                        Return result
                    End If

                End If
            Next

            'Not found
            addNodeNamingError("CCCC01", "The <" & parentType.Name & "> class does Not contain a """ & searchName & """ propertie", node.childNode)

        ElseIf TypeOf node.childNode Is FunctionCallNode Then

            'Variables
            Dim funCall As FunctionCallNode = DirectCast(node.childNode, FunctionCallNode)
            Dim searchName As String = funCall.FunctionName

            'Search method
            For Each fun As FunctionNode In parentType.methods

                'Not the one
                If Not fun.Name = searchName Then
                    Continue For
                End If

                'Handle argument error
                If funCall.Arguments.Count < fun.minArguments Then
                    addNodeTypeError("CCCC03", (fun.Arguments.Count - funCall.Arguments.Count).ToString() & " arguments are missing", node)
                End If
                If funCall.Arguments.Count > fun.maxArguments Then
                    addNodeTypeError("CCCC04", (funCall.Arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
                End If

                'Argument
                Dim arguments As String = ""
                For i As Integer = 0 To fun.Arguments.Count - 1

                    'Fill optionnal argument
                    If i >= funCall.Arguments.Count Then
                        arguments &= ", NULL"
                        Continue For
                    End If

                    'Variables
                    Dim argumentModel As FunctionArgument = fun.Arguments(i)
                    Dim argumentValue As Node = funCall.Arguments(i)

                    'Handle type error
                    If Not (argumentModel.compiledType = getNodeType(argumentValue, context)) Then
                        addNodeTypeError("CCCC05", "The " & (i + 1).ToString() & " argument is of type <" & getNodeType(argumentValue, context).ToString() & "> instead of being <" & argumentModel.type.ToString() & ">", argumentValue)
                    End If

                    'Add argument
                    If argumentModel.doubleRef Then
                        If (TypeOf argumentValue IsNot VariableNode) Then
                            addNodeTypeError("CCCCC06", "This argument can only take variables as its value.", argumentValue)
                        End If
                        arguments &= ", &(" & compileNode(argumentValue, content, context) & ")"
                    Else
                        arguments &= ", " & compileNode(argumentValue, content, context)
                    End If

                Next

                If node.allLine Then
                    content.Add(String.Format("{0}({1}{2});", fun.compiledName, compileNode(node.parentStruct, content, context), arguments))
                    Return ""
                Else
                    Return String.Format("{0}({1}{2})", fun.compiledName, compileNode(node.parentStruct, content, context), arguments)
                End If

            Next

            'Not found
            addNodeNamingError("CCCC02", "The <" & parentType.Name & "> Class does Not contain a """ & searchName & """ method", node.childNode)

        End If

        'Return
        Return ""

    End Function

    '=================================
    '========== COMPILE NEW ==========
    '=================================
    Private Function compileNew(ByVal node As newNode, ByVal content As List(Of String), ByVal context As context) As String

        'Get class
        Dim targetType As Type = compileTypeNode(node.type, context)

        'Get constructor
        Dim fun As FunctionNode = Nothing
        For Each method As FunctionNode In targetType.methods
            If method.Name = "new" Then
                fun = method
            End If
        Next
        If fun Is Nothing Then
            addNodeSyntaxError("CCCN01", "Class <" & targetType.Name & "> does not contain a constructor", node, "Add a ""new"" method to the class")
        End If

        'Handle argument error
        If node.arguments.Count < fun.minArguments Then
            addNodeSyntaxError("CCCN02", (fun.Arguments.Count - node.arguments.Count).ToString() & " arguments are missing", node)
        End If
        If node.arguments.Count > fun.maxArguments Then
            addNodeSyntaxError("CCCN03", (node.arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
        End If

        'Argument
        Dim arguments As String = ""
        For i As Integer = 0 To fun.Arguments.Count - 1

            'Fill optionnal argument
            If i >= node.arguments.Count Then
                arguments &= ", NULL"
                Continue For
            End If

            'Variables
            Dim argumentModel As FunctionArgument = fun.Arguments(i)
            Dim argumentValue As Node = node.arguments(i)

            'Handle type error
            If Not (argumentModel.compiledType = getNodeType(argumentValue, context)) Then
                addNodeTypeError("CCCN04", "The " & (i + 1).ToString() & " argument is of type <" & getNodeType(argumentValue, context).ToString() & "> instead of being <" & argumentModel.type.ToString() & ">", argumentValue)
            End If

            'Add argument
            If argumentModel.doubleRef Then
                If (TypeOf argumentValue IsNot VariableNode) Then
                    addNodeTypeError("CCCNC05", "This argument can only take variables as its value.", argumentValue)
                End If
                arguments &= ", &(" & compileNode(argumentValue, content, context) & ")"
            Else
                arguments &= ", " & compileNode(argumentValue, content, context)
            End If

        Next
        If arguments.StartsWith(", ") Then
            arguments = arguments.Substring(2)
        End If

        'Return
        Return String.Format("{0}({1})", fun.compiledName, arguments)

    End Function

    '==================================
    '========== COMPILE LIST ==========
    '==================================
    Public Function compileList(ByVal node As ListNode, ByVal content As List(Of String), ByVal context As context) As String

        'Handle no value
        If node.elements.Count = 0 Then
            addNodeTypeError("CCCL01", "A list cannot be empty, as this does not identify its type.", node)
        End If

        'Get type
        Dim listItemType As Type = getNodeType(node.elements(0), context)

        'Compile each arguments
        Dim helpVar As String = getHelpVariableName()
        content.Add(String.Format("{0} * {1};", "", helpVar))
        For Each elm As Node In node.elements

            'Handle type error
            Dim elmType As Type = getNodeType(elm, context)
            If Not listItemType = elmType Then
                addNodeTypeError("CCCL02", "The list is a list of <" & listItemType.ToString() & "> elements, but this element is of type <" & elmType.ToString() & ">", elm)
            End If

            'Compile
            content.Add(String.Format("{0}.Add({1})", helpVar, compileNode(elm, content, context)))

        Next

        'Finish compile
        Return helpVar

    End Function

    '==================================
    '========== COMPILE NODE ==========
    '==================================
    Private Function compileNode(ByVal node As Node, ByVal content As List(Of String), ByVal context As context) As String

        If TypeOf node Is DeclareVariableNode Then

            'Compile
            Return compileDeclareVariable(DirectCast(node, DeclareVariableNode), content, context)

        ElseIf TypeOf node Is ListNode Then

            'Compile
            Return compileList(DirectCast(node, ListNode), content, context)

        ElseIf TypeOf node Is newNode Then

            'Compile
            Return compileNew(DirectCast(node, newNode), content, context)

        ElseIf TypeOf node Is SetVariableNode Then

            'Compîle
            Return compileSetVariable(DirectCast(node, SetVariableNode), content, context)

        ElseIf TypeOf node Is childNode Then

            'Compile
            Return compileChild(DirectCast(node, childNode), content, context)

        ElseIf TypeOf node Is FunctionCallNode Then

            'Compîle
            Return compileFunctionCall(DirectCast(node, FunctionCallNode), content, context)

        ElseIf TypeOf node Is whileStatementNode Then

            'Compile
            Return compileWhileStatement(DirectCast(node, whileStatementNode), content, context)

        ElseIf TypeOf node Is valueNode Then

            'Compile
            Return compileValue(DirectCast(node, valueNode), content, context)

        ElseIf TypeOf node Is AddSourceNode Then

            'Compile
            Return compileAddSource(DirectCast(node, AddSourceNode), content, context)


        ElseIf TypeOf node Is BooleanNode Then

            'Compile
            Return compileBoolean(DirectCast(node, BooleanNode))


        ElseIf TypeOf node Is StringNode Then

            'Compile
            Return compileString(DirectCast(node, StringNode))


        ElseIf TypeOf node Is VariableNode Then

            'Compile
            Return compileVariable(DirectCast(node, VariableNode), content, context)


        End If

        'Return
        addNodeTypeError("CCCN01", "Unable to resolve node type", node)
        Return Nothing

    End Function

    '===================================
    '========== GET NODE TYPE ==========
    '===================================
    Private Function getNodeType(ByVal node As Node, ByVal context As context) As Type

        'Unsafe type
        If TypeOf node Is typeNode Then

            'Return
            Return compileTypeNode(DirectCast(node, typeNode), context)

        End If

        'StringNode
        If TypeOf node Is StringNode Then

            'Return
            Return stdStr

        End If

        'ListNode
        If TypeOf node Is ListNode Then

            'Castednode
            Dim castedNode As ListNode = DirectCast(node, ListNode)

            'Handle no value
            If castedNode.elements.Count = 0 Then
                addNodeTypeError("CCGNT04", "A list cannot be empty, as this does not identify its type.", node)
            End If

            'Get type
            Return getNodeType(castedNode.elements(0), context)

        End If

        'New node
        If TypeOf node Is newNode Then

            'Castednode
            Dim castedNode As newNode = DirectCast(node, newNode)

            'Get type
            Return compileTypeNode(castedNode.type, context)

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

        'Variable node
        If TypeOf node Is VariableNode Then

            'Return
            Return getVariable(DirectCast(node, VariableNode).VariableName, context).Item1.type

        End If

        'FunctionCallNode
        If TypeOf node Is FunctionCallNode Then

            'Castednode
            Dim castedNode As FunctionCallNode = DirectCast(node, FunctionCallNode)

            'Get variable
            Dim func As FunctionNode = getFunction(castedNode.FunctionName, castedNode)

            'Return
            Return func.ReturnType

        End If


        'ChildNode
        If TypeOf node Is childNode Then

            'Castednode
            Dim castedNode As childNode = DirectCast(node, childNode)

            'Get class
            Dim parentType As Type = getNodeType(castedNode.parentStruct, context)

            'Get type
            If TypeOf castedNode.childNode Is VariableNode Then

                'Variables
                Dim searchName As String = DirectCast(castedNode.childNode, VariableNode).VariableName

                'Search propertie
                For Each var As Variable In parentType.variables
                    If var.name = searchName Then
                        Return var.type
                    End If
                Next

                'Not found
                addNodeNamingError("CCGNT06", "The <" & parentType.Name & "> class does not contain a """ & searchName & """ propertie", castedNode.childNode)

            ElseIf TypeOf castedNode.childNode Is FunctionCallNode Then

                'Variables
                Dim searchName As String = DirectCast(castedNode.childNode, FunctionCallNode).FunctionName

                'Search method
                For Each fun As FunctionNode In parentType.methods
                    If fun.Name = searchName Then
                        Return fun.ReturnType
                    End If
                Next

                'Not found
                addNodeNamingError("CCGNT07", "The <" & parentType.Name & "> class does not contain a """ & searchName & """ method", castedNode.childNode)

            Else

                Throw New NotImplementedException()

            End If

        End If

        'Boolean stuff
        If TypeOf node Is BooleanNode Or TypeOf node Is ComparisonNode Then

            'Return
            Return stdBool

        End If

        'Return
        addNodeTypeError("CCGNT01", "Unable to resolve node type", node)
        Return Nothing

    End Function

End Class