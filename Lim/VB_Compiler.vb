Imports System.IO
Public Class VB_Compiler

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public files As New List(Of LimFile)
    Private entryFile As LimFile

    Private compiledClasss As String
    Private compiledFunctions As String
    Private compiledVariables As String

    Private CompiledNameAlphabet As String = "abcdefghijklmnoqrstuvwxyz"
    Private variableCount As Integer = 0
    Private helpVariableCount As Integer = 0
    Private functionCount As Integer = 0
    Private classCount As Integer = 0

    Private compileType As compileWay
    Public graphicsDrawFunction As FunctionNode

    Private stdInt As ClassNode
    Private stdFloat As ClassNode
    Private stdStr As ClassNode
    Private stdBool As ClassNode
    Private stdFun As ClassNode

    Private logs As Boolean = False

    '==============================
    '========== RUN CODE ==========
    '==============================
    Public Sub runCode(ByVal inputFile As String, ByVal workingDirectory As String, ByVal flags As List(Of String))

        'Compile
        compileCode(inputFile)

        'Run
        Dim run As New Process()
        run.StartInfo.FileName = "dotnet"
        run.StartInfo.Arguments = "run"
        run.StartInfo.WorkingDirectory = AppData & "/compiled"
        run.Start()

        'Wait until programs end
        While Not run.HasExited
            Threading.Thread.Sleep(200)
        End While

    End Sub

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Sub compile(ByVal inputFile As String, ByVal outputPath As String, ByVal flags As List(Of String))

        'Compile
        compileCode(inputFile, True)

        'Flags
        Dim debugFlag As Boolean = (flags.Contains("-d") Or flags.Contains("--debug"))

        'Set environment
        If Directory.Exists(AppData & "/publish") Then
            Try
                Directory.Delete(AppData & "/publish", True)
            Catch ex As Exception
                addBasicError("Can't delete folder", ex.Message)
            End Try
        End If
        Try
            Directory.CreateDirectory(AppData & "/publish")
        Catch ex As Exception
            addBasicError("unable to create folder", ex.Message)
        End Try
        addLog("Renew dotnet environment: OK")

        Dim dotnetCompiler As New Process()

        dotnetCompiler.StartInfo.FileName = "cmd.exe"
        'dotnetCompiler.StartInfo.Arguments = "/c cd """ & AppData & """ & dotnet publish compiled/VB.vbproj --configuration final --framework net6.0 --self-contained True --output publish --runtime win-x64 --verbosity Normal /Property:PublishTrimmed=False /property:PublishSingleFile=True /property:IncludeNativeLibrariesForSelfExtract=True /property:DebugType=None /property:DebugSymbols=False /property:EnableCompressionInSingleFile=True"
        dotnetCompiler.StartInfo.Arguments = "/c cd """ & AppData & "/compiled"" & dotnet publish"

        If Not debugFlag Then
            dotnetCompiler.StartInfo.Arguments &= ">nul"
        End If

        dotnetCompiler.Start()

        'Wait
        While Not dotnetCompiler.HasExited
            If Not debugFlag Then
                Console.Write(".")
            End If
            Threading.Thread.Sleep(200)
        End While
        Console.Write(Environment.NewLine)

        'Final stage
        If Not dotnetCompiler.ExitCode = 0 Then
            'Error
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine("La compilation a réussi à échouer")
            Console.ResetColor()
            Console.WriteLine("An error occurred during the final stage of the compilation. Try again in another environment. If the problem persists please contact the developers.")
            endApp()
        End If

        'Move executable
        Try
            'File.Move(AppData & "/publish/VB.exe", outputPath, True)
            File.Move(AppData & "/compiled/bin/Debug/net6.0-windows/win-x64/publish/VB.exe", outputPath, True)
        Catch ex As Exception
            addBasicError("Unable to move a file", ex.Message)
        End Try

        'Sucess
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("Compilation successful.")
        Console.ResetColor()

    End Sub

    '==================================
    '========== COMPILE CODE ==========
    '==================================
    Public Sub compileCode(ByVal inputFile As String, Optional logs As Boolean = False)

        'Restore variables
        compiledVariables = ""
        compiledClasss = ""
        compiledFunctions = ""
        variableCount = 0
        helpVariableCount = 0
        functionCount = 0
        classCount = 0
        compileType = compileWay.console
        graphicsDrawFunction = Nothing
        Me.logs = logs
        Dim outputFolder As String = AppData & "/compiled"

        'Get template
        Dim vbTemplate As String = templateFolder & "/vb"
        If Not Directory.Exists(vbTemplate) Then
            addBasicError("Folder not found", "The ""templates/vb"" folder could not be found. Try reinstalling lim.")
        End If

        'Add dimensions
        compiledClasss &= Environment.NewLine & Environment.NewLine & Helper.getTemplate(vbTemplate, "dimensions.vb")

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

        'Get std's classs
        stdInt = getClass("int", entryFile, False)
        stdFloat = getClass("float", entryFile, False)
        stdStr = getClass("str", entryFile, False)
        stdBool = getClass("bool", entryFile, False)
        stdFun = getClass("fun", entryFile, False)

        'Compiles classs
        compileClass(stdInt)
        compileClass(stdFloat)
        compileClass(stdBool)
        compileClass(stdBool)
        compileClass(stdFun)

        'Get graphics
        If graphicsDrawFunction IsNot Nothing Then
            compileType = compileWay.window
        End If

        'Logs
        If logs Then
            addLog("Load the ""std"" library: OK")
        End If

        'Compile start
        compileFunction(entryPoint)

        'Compile drawFrame
        If compileType = compileWay.window Then
            If graphicsDrawFunction Is Nothing Then
                addBasicError("Graphics lib", "The ""drawFrame"" function was not found")
            End If
            compileFunction(graphicsDrawFunction)
        End If

        'Compile limlibs
        Dim CompiledAddSourceDirectly As String = ""
        For Each file As LimFile In files

            If file.LimLib Then

                'File has sources variables
                If file.addSourceDirectly.Count > 0 Then

                    CompiledAddSourceDirectly &= Environment.NewLine & "'" & file.name
                    For Each source As AddSourceNode In file.addSourceDirectly
                        CompiledAddSourceDirectly &= Environment.NewLine & source.value
                    Next

                End If

            End If

        Next

        'Get final code
        Dim finalCode As String = ""
        If Not CompiledAddSourceDirectly = "" Then
            finalCode &= Environment.NewLine & Environment.NewLine & vbTab & "'///////////////////////////" & Environment.NewLine & vbTab & "'///////// LIMLIBS /////////" & Environment.NewLine & vbTab & "'///////////////////////////" & CompiledAddSourceDirectly.Replace(Environment.NewLine, Environment.NewLine & vbTab)
        End If
        If Not compiledVariables = "" Then
            finalCode &= Environment.NewLine & Environment.NewLine & vbTab & "'///////////////////////////" & Environment.NewLine & vbTab & "'//////// VARIABLES ////////" & Environment.NewLine & vbTab & "'///////////////////////////" & compiledVariables.Replace(Environment.NewLine, Environment.NewLine & vbTab)
        End If
        If Not compiledClasss = "" Then
            finalCode &= Environment.NewLine & Environment.NewLine & vbTab & "'///////////////////////////" & Environment.NewLine & vbTab & "'////////// CLASS //////////" & Environment.NewLine & vbTab & "'///////////////////////////" & compiledClasss.Replace(Environment.NewLine, Environment.NewLine & vbTab)
        End If
        If Not compiledFunctions = "" Then
            finalCode &= Environment.NewLine & Environment.NewLine & vbTab & "'///////////////////////////" & Environment.NewLine & vbTab & "'//////// FUNCTIONS ////////" & Environment.NewLine & vbTab & "'///////////////////////////" & compiledFunctions.Replace(Environment.NewLine, Environment.NewLine & vbTab)
        End If
        finalCode = finalCode.Replace(".clone().clone()", ".clone()")
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

        'Get template
        Dim folderTemplatePath As String = ""
        Select Case compileType

            Case compileWay.console
                folderTemplatePath = "console"

            Case compileWay.window
                folderTemplatePath = "window"

        End Select
        If Not Directory.Exists(vbTemplate & "/compileEnvironment/" & folderTemplatePath) Then
            addBasicError("File missing", "The """ & folderTemplatePath & """ model folder could not be found. Try reinstalling Lim.")
        End If

        'Copy template;
        Microsoft.VisualBasic.FileIO.FileSystem.CopyDirectory(vbTemplate & "/compileEnvironment/" & folderTemplatePath, outputFolder)
        If logs Then
            addLog("Renew the environment: OK")
        End If

        'Check template file
        If Not File.Exists(outputFolder & "/Program.vb") Then
            addBasicError("File missing", """Program.vb"" is missing")
        End If

        'Write
        Dim Program As String = ""
        Try
            Program = File.ReadAllText(outputFolder & "/Program.vb")
        Catch ex As Exception
            addBasicError("Cannot read file", ex.Message)
        End Try

        'Add code
        Program = Program.Replace("'{CODES}'", finalCode)
        Program = Program.Replace("'{ENTRY_POINT}'", entryPoint.compiledName & "()")
        Select Case compileType

            Case compileWay.window
                Program = Program.Replace("'{DRAWFRAME}'", graphicsDrawFunction.compiledName)

        End Select

        'Write file
        Try
            File.WriteAllText(outputFolder & "/Program.vb", Program)
        Catch ex As Exception
            addBasicError("Cannot write file", ex.Message)
        End Try
        If logs Then
            addLog("Writing the final file: OK")
        End If

    End Sub

    '=================================
    '========== COMPILE WAY ==========
    '=================================
    Private Enum compileWay
        console
        window
    End Enum

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

    '======================================
    '========== TYPENODE TO TYPE ==========
    '======================================
    Private Function typenodeToSafeType(ByVal typenode As typeNode) As safeType

        Dim TypeClass As ClassNode = getClass(typenode.className, typenode)
        compileClass(TypeClass)
        Return New safeType(TypeClass, typenode.Dimensions)

    End Function

    '===============================
    '========== GET CLASS ==========
    '===============================
    Public Function getClass(ByVal name As String, ByVal nodeCaller As Node, Optional ByVal compileClass As Boolean = True) As ClassNode

        'Get file
        Dim currentFile As LimFile = getNodeParentFile(nodeCaller)

        'Search in Current file
        For Each currentClass As ClassNode In currentFile.classs
            If currentClass.Name = name Then
                If compileClass Then
                    Me.compileClass(currentClass)
                End If
                Return currentClass
            End If
        Next

        'Search in others files
        For Each file As LimFile In currentFile.FilesImports
            For Each currentClass As ClassNode In file.classs
                If currentClass.Name = name And currentClass.export Then
                    If compileClass Then
                        Me.compileClass(currentClass)
                    End If
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
            If declareVariable.variableName = name Then

                'Create content
                Dim content As New List(Of String)

                'Declare variable
                content.Add(String.Format("'{0}", declareVariable.variableName))

                'Set variable
                Dim compiledVariableName As String
                If currentFile.LimLib Then
                    compiledVariableName = declareVariable.variableName
                Else
                    compiledVariableName = getVariableName()
                End If
                Dim var As New Variable(declareVariable.variableName, Nothing, compiledVariableName, declareVariable.declarationType)

                'Get type
                If declareVariable.value Is Nothing Then

                    'Set type
                    var.type = typenodeToSafeType(declareVariable.variableUnsafeType)

                    'List or something
                    If var.type.Dimensions.Count > 0 Then
                        'New
                        content.Add(String.Format("Dim {0} As New {1}", var.compiledName, compileSafeType(var.type)))
                    Else
                        'Normal
                        content.Add(String.Format("Dim {0} As {1} = Nothing", var.compiledName, compileSafeType(var.type)))
                    End If

                Else

                    'Set type
                    var.type = getNodeType(declareVariable.value)
                    If Not declareVariable.variableUnsafeType Is Nothing Then
                        Dim defType As safeType = typenodeToSafeType(declareVariable.variableUnsafeType)
                        If Not var.type.IsTheSameAs(defType) Then
                            addNodeTypeError("VBCCF02", "The defined type of the variable is not the same as that of its value.", declareVariable.value)
                        End If
                    End If

                    'Ref
                    Select Case var.declarationType

                        Case VariableDeclarationType._let_
                            content.Add(String.Format("Dim {0} As {1} = {2}", var.compiledName, compileSafeType(var.type), compileNode(declareVariable.value, content)))

                        Case VariableDeclarationType._var_
                            content.Add(String.Format("Dim {0} As {1} = {2}.clone()", var.compiledName, compileSafeType(var.type), compileNode(declareVariable.value, content)))

                        Case Else
                            Throw New NotImplementedException

                    End Select

                End If

                'Add variable
                currentFile.variables.Add(var)
                Dim final As String = ""
                For Each line As String In content
                    If Not line = "" Then
                        final &= Environment.NewLine & line
                    End If
                Next
                compiledVariables &= Environment.NewLine & final
                Return var

            End If
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
                If declareVariable.variableName = name And declareVariable.export Then

                    'Create content
                    Dim content As New List(Of String)

                    'Declare variable
                    content.Add(String.Format("'{0}", declareVariable.variableName))

                    'Set variable
                    Dim compiledVariableName As String
                    If currentFile.LimLib Then
                        compiledVariableName = declareVariable.variableName
                    Else
                        compiledVariableName = getVariableName()
                    End If
                    Dim var As New Variable(declareVariable.variableName, Nothing, compiledVariableName, declareVariable.declarationType)

                    'Get type
                    If declareVariable.value Is Nothing Then

                        'Set type
                        var.type = typenodeToSafeType(declareVariable.variableUnsafeType)

                        'List or something
                        If var.type.Dimensions.Count > 0 Then
                            'New
                            content.Add(String.Format("Dim {0} As New {1}", var.compiledName, compileSafeType(var.type)))
                        Else
                            'Normal
                            content.Add(String.Format("Dim {0} As {1} = Nothing", var.compiledName, compileSafeType(var.type)))
                        End If

                    Else

                        'Set type
                        var.type = getNodeType(declareVariable.value)
                        If Not declareVariable.variableUnsafeType Is Nothing Then
                            Dim defType As safeType = typenodeToSafeType(declareVariable.variableUnsafeType)
                            If Not var.type.IsTheSameAs(defType) Then
                                addNodeTypeError("VBCCF02", "The defined type of the variable is not the same as that of its value.", declareVariable.value)
                            End If
                        End If

                        'Ref
                        Select Case var.declarationType

                            Case VariableDeclarationType._let_
                                content.Add(String.Format("Dim {0} As {1} = {2}", var.compiledName, compileSafeType(var.type), compileNode(declareVariable.value, content)))

                            Case VariableDeclarationType._var_
                                content.Add(String.Format("Dim {0} As {1} = {2}.clone()", var.compiledName, compileSafeType(var.type), compileNode(declareVariable.value, content)))

                            Case Else
                                Throw New NotImplementedException

                        End Select

                    End If

                    'Add variable
                    currentFile.variables.Add(var)
                    Dim final As String = ""
                    For Each line As String In content
                        If Not line = "" Then
                            final &= Environment.NewLine & line
                        End If
                    Next
                    compiledVariables &= Environment.NewLine & final
                    Return var

                End If
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

        'TODO: Variable that contains functions

        'Anable to find the class
        addNodeNamingError("VBCGF01", "The function """ & name & """ could not be found.", nodeCaller, "Check the function name or that the file is correctly imported.")
        Return Nothing

    End Function

    '==================================
    '========== COMPILE TYPE ==========
    '==================================
    Private Function compileSafeType(ByVal type As safeType) As String

        'Variable
        Dim result As String

        'Compile
        If type.Dimensions.Count > 0 Then
            If type.Dimensions(type.Dimensions.Count - 1) = ValueType.list Then
                result = String.Format("LimList(Of {0})", compileSafeType(type.getParentType()))
            ElseIf type.Dimensions(type.Dimensions.Count - 1) = ValueType.map Then
                result = String.Format("LimMap(Of {0})", compileSafeType(type.getParentType()))
            Else
                Throw New NotImplementedException()
            End If
        Else
            result = type.TargetClass.compiledName
        End If

        'Return
        Return result

    End Function

    '===================================
    '========== COMPILE CLASS ==========
    '===================================
    Private Sub compileClass(ByRef currentClass As ClassNode)

        'State handler
        If currentClass.compiled Then
            Exit Sub
        End If
        currentClass.compiled = True

        'Get name
        If currentClass.compiledName = "" Then
            If getNodeParentFile(currentClass).LimLib Then
                currentClass.compiledName = currentClass.Name
            Else
                currentClass.compiledName = getClassName()
            End If
        End If

        'Some variables
        Dim content As New List(Of String)
        Dim propreties_init As New List(Of String)

        'Add source directly
        If currentClass.addSourceDirectly.Count > 0 Then
            content.Add("")
            content.Add("'////// ADD SOURCE DIRECTLY //////")
            For Each source As AddSourceNode In currentClass.addSourceDirectly
                content.Add(source.value)
            Next
        End If

        'Get variables
        If currentClass.declareVariables.Count > 0 Then

            content.Add("")
            content.Add("'////// VARIABLES //////")
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
                    content.Add(String.Format("Public {0} As {1}", var.compiledName, compileSafeType(var.type)))
                    propreties_init.Add(String.Format("{0g} = {1}", var.compiledName, compileNode(def.value, propreties_init)))

                Else

                    'Set type
                    var.type = typenodeToSafeType(def.variableUnsafeType)

                    'Compile
                    content.Add(String.Format("Public {0} As {1}", var.compiledName, compileSafeType(var.type)))

                End If

                'Add variable
                currentClass.variables.Add(var)

            Next

        End If

        'Has function
        Dim hasStr As Boolean = False
        Dim hasCreate As Boolean = False

        'Get functions
        If currentClass.methods.Count > 0 Then
            content.Add("")
            content.Add("'////// METHODS //////")
            For Each def As FunctionNode In currentClass.methods

                'Compile
                content.Add("")
                content.Add(compileFunction(def).Replace(Environment.NewLine, Environment.NewLine & vbTab))

                'Type
                If def.Name = "str" Then
                    hasStr = True
                ElseIf def.Name = "create" Then
                    hasCreate = True
                End If

            Next
        End If

        'Str function
        If Not hasStr Then
            Dim str_method As FunctionNode = New FunctionNode(0, 0, "str", New List(Of FunctionArgument), New typeNode(0, 0, "str", New List(Of ValueType)))
            str_method.compiled = True
            str_method.parentNode = currentClass
            str_method.compiledName = "__str__"
            str_method.ReturnType = New safeType(stdStr)
            currentClass.methods.Add(str_method)
            content.Add("")
            content.Add("'str()")
            content.Add("Public Function __str__() As str")
            content.Add("   Return New str(""<" & currentClass.Name & ">"")")
            content.Add("End Function")
        End If

        'Create function
        If Not hasCreate And Not getNodeParentFile(currentClass).LimLib Then
            Dim create_method As FunctionNode = New FunctionNode(0, 0, "create", New List(Of FunctionArgument), Nothing)
            create_method.compiled = True
            create_method.parentNode = currentClass
            create_method.compiledName = "New"
            create_method.ReturnType = Nothing
            currentClass.methods.Add(create_method)
            content.Add("")
            content.Add("'Create")
            content.Add("Public Sub New()")
            content.Add("   Me.load_properties()")
            content.Add("End Sub")
        End If

        'To String
        content.Add("")
        content.Add("'ToString")
        content.Add("Public Overrides Function ToString() As String")
        content.Add("   Return Me.__str__().value")
        content.Add("End Function")

        'Load properties
        content.Add("")
        content.Add("'////// LOAD PROPERTIES //////")
        content.Add("Private Sub load_properties()")
        For Each line As String In propreties_init
            content.Add(vbTab & line)
        Next
        content.Add("End Sub")

        'Relations
        If currentClass.relations.Count > 0 Then
            content.Add("")
            content.Add("'////// RELATIONS //////")
            For Each def As RelationNode In currentClass.relations

                'Handle type
                If Not def.Arguments(0).type.className = currentClass.Name Then
                    addNodeSyntaxError("VBCS02", "A relation must have as first argument, itself.", def.Arguments(0).type, "Change argument type to "":" & currentClass.Name & """")
                End If

                'Compile
                content.Add("")
                content.Add(compileRelation(def).Replace(Environment.NewLine, Environment.NewLine & vbTab))

            Next
        End If

        'Final
        Dim finalString As String = "Public Class " & currentClass.compiledName
        For i As Integer = 0 To content.Count - 1
            finalString &= Environment.NewLine & vbTab & content(i)
        Next
        finalString &= Environment.NewLine & Environment.NewLine & "End Class"
        compiledClasss &= Environment.NewLine & Environment.NewLine & "'" & currentClass.Name & Environment.NewLine & finalString

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
        If Not fun.unsafeReturnType Is Nothing Then
            fun.ReturnType = typenodeToSafeType(fun.unsafeReturnType)
        End If

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

            'Custom method
            If Not parentClass Is Nothing Then

                If fun.Name = "create" Then
                    'New
                    fun.compiledName = "New"
                    content.Add("'Load properties")
                    content.Add("Me.load_properties()")
                    content.Add("")
                    content.Add("'Content")

                ElseIf fun.Name = "str" Then
                    '__str__
                    fun.compiledName = "__str__"

                End If
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

            Dim var As New Variable(arg.name, typenodeToSafeType(arg.type), compiledName, arg.declareType)
            fun.variables.Add(var)

            If arg.value IsNot Nothing Then

                If Not ValueIsIndependent(arg.value) Then
                    addNodeTypeError("VBCF01", "The value indicated as an optional parameter must be independent.", arg.value, "Put a default value, then, check in the function if the value of the argument is the default one. If yes, set the complex value.")
                End If

                compiled_arguments &= ", Optional " & var.compiledName & " As " & compileSafeType(var.type) & " = Nothing"
                content.Add(String.Format("If {0} Is Nothing Then", var.compiledName))
                content.Add(String.Format(" {0} = {1}", var.compiledName, compileNode(arg.value, content)))
                content.Add("End If")

            Else

                compiled_arguments &= ", " & var.compiledName & " As " & compileSafeType(var.type)

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

        'Final
        Dim finalString As String
        If fun.ReturnType Is Nothing Then
            finalString = "Public Sub " & fun.compiledName & "(" & compiled_arguments & ")" & Environment.NewLine
        Else
            finalString = "Public Function " & fun.compiledName & "(" & compiled_arguments & ") As " & compileSafeType(fun.ReturnType) & Environment.NewLine
        End If
        For i As Integer = 0 To content.Count - 1
            finalString &= Environment.NewLine & vbTab & content(i)
        Next
        If fun.ReturnType Is Nothing Then
            finalString &= Environment.NewLine & Environment.NewLine & "End Sub"
        Else
            If Not parentClass Is Nothing And fun.Name = "clone" Then
                finalString &= Environment.NewLine & Environment.NewLine & vbTab & "'Return basic copy" & Environment.NewLine & vbTab & "Return DirectCast(Me.MemberwiseClone(), " & compileSafeType(fun.ReturnType) & ")" & Environment.NewLine & Environment.NewLine & "End Function"
            Else
                finalString &= Environment.NewLine & Environment.NewLine & vbTab & "'Return empty" & Environment.NewLine & vbTab & "Return Nothing" & Environment.NewLine & Environment.NewLine & "End Function"
            End If
        End If
        finalString = "'" & fun.Name & Environment.NewLine & finalString

        'End compiling
        fun.compiling = False

        'Return
        If parentClass Is Nothing Then
            compiledFunctions &= Environment.NewLine & Environment.NewLine & finalString
            Return ""
        Else
            Return finalString
        End If

    End Function

    '======================================
    '========== COMPILE RELATION ==========
    '======================================
    Public Function compileRelation(ByVal relation As RelationNode) As String

        'Get unsafe type
        If Not relation.unsafeReturnType Is Nothing Then
            relation.ReturnType = typenodeToSafeType(relation.unsafeReturnType)
        End If

        'Some variables
        Dim content As New List(Of String)
        Dim parentFile As LimFile = getNodeParentFile(relation)

        'Get operator
        Dim operatorString As String = ""
        Select Case relation.operator_name.type

            Case tokenType.OP_PLUS
                operatorString = "+"

            Case tokenType.OP_MINUS
                operatorString = "-"

            Case tokenType.OP_MULTIPLICATION
                operatorString = "*"

            Case tokenType.OP_DIVISION
                operatorString = "/"

            Case tokenType.OP_MODULO
                operatorString = "Mod"

            Case Else
                addSyntaxError("VBCR02", "The compiler does not recognize this operator despite the fact that it is defined from a relation.", parentFile, relation.operator_name.positionStart, relation.operator_name.positionEnd)

        End Select

        'Argument list
        Dim compiled_arguments As String = ""
        For Each arg As FunctionArgument In relation.Arguments

            Dim compiledName As String
            If parentFile.LimLib Then
                compiledName = arg.name
            Else
                compiledName = getVariableName()
            End If

            Dim var As New Variable(arg.name, typenodeToSafeType(arg.type), compiledName, arg.declareType)
            relation.variables.Add(var)

            compiled_arguments &= ", " & var.compiledName & " As " & compileSafeType(var.type)

        Next
        If compiled_arguments.StartsWith(", ") Then
            compiled_arguments = compiled_arguments.Substring(2)
        End If

        'Compile content
        For Each action As Node In relation.codes

            Dim compiledAction As String = compileNode(action, content)
            If Not compiledAction = "" Then
                content.Add(compiledAction)
            End If

        Next

        'Return is empty
        If relation.ReturnType Is Nothing Then
            addNodeSyntaxError("VBCR01", "A relation must absolutely return a value", relation)
        End If

        'Final
        Dim finalString As String = ""
        finalString = "Public Shared Operator " & operatorString & "(" & compiled_arguments & ") As " & compileSafeType(relation.ReturnType)
        For i As Integer = 0 To content.Count - 1
            finalString &= Environment.NewLine & vbTab & content(i)
        Next
        finalString &= Environment.NewLine & vbTab & ""
        finalString &= "Throw New Exception(""Relation doesn't return anything (" & parentFile.name & ", line " & getLinePosition(parentFile.content, relation.positionStart).ToString() & ")"")"
        finalString &= Environment.NewLine & vbTab & "Return Nothing"
        finalString &= Environment.NewLine & "End Operator"

        'Return
        Return finalString

    End Function

    '===================================
    '========== GET NODE TYPE ==========
    '===================================
    Private Function getNodeType(ByVal node As Node) As safeType

        'Unsafe type
        If TypeOf node Is typeNode Then

            'Return
            Return typenodeToSafeType(DirectCast(node, typeNode))

        End If

        'valueNode
        If TypeOf node Is valueNode Then

            'Casted Node
            Dim castedNode As valueNode = DirectCast(node, valueNode)

            'Return
            Select Case castedNode.tok.type
                Case tokenType.CT_INTEGER
                    Return New safeType(stdInt)

                Case tokenType.CT_FLOAT
                    Return New safeType(stdFloat)

                Case Else
                    addBasicError("Type error", "Cannot get type of the folowing token <" & castedNode.tok.ToString() & ">")
                    Return Nothing

            End Select

        End If

        'UnaryOpNode
        If TypeOf node Is UnaryOpNode Then

            'Casted node
            Dim castedNode As UnaryOpNode = DirectCast(node, UnaryOpNode)

            'Return
            Select Case castedNode.op.type
                Case tokenType.OP_MINUS
                    Return getNodeType(castedNode.node)

                Case tokenType.OP_PLUS
                    Return getNodeType(castedNode.node)

                Case Else
                    addBasicError("Type error", "Cannot get type of the folowing token <" & castedNode.op.ToString() & ">")
                    Return Nothing

            End Select
            Dim t As String = "".Substring(0, 1)
        End If

        'binOpNode
        If TypeOf node Is binOpNode Then

            'Casted node
            Dim castedNode As binOpNode = DirectCast(node, binOpNode)

            'Get type
            Dim left As safeType = getNodeType(castedNode.leftNode)
            Dim right As safeType = getNodeType(castedNode.rightNode)

            'Get relation
            For Each relation As RelationNode In left.TargetClass.relations

                'Get argument type
                Dim relationRightArgumentType As safeType = typenodeToSafeType(relation.Arguments(1).type)

                'If it's not the same
                If Not relationRightArgumentType.IsTheSameAs(right) Then
                    Continue For
                End If

                'Compile
                Return relation.ReturnType

            Next

            'Error
            addNodeTypeError("VBGNT17", "The operation between an <" & left.ToString() & "> type and a <" & right.ToString() & "> type is undefined.", castedNode, "Create a ""relation"" of these two types in the class <" & left.TargetClass.Name & ">")

            ''Handle
            'If left.Dimensions.Count > 0 Then
            '    addNodeTypeError("VBGNT02", "The subtraction operation cannot be performed on a list", castedNode.leftNode)
            'End If
            'If right.Dimensions.Count > 0 Then
            '    addNodeTypeError("VBGNT03", "The subtraction operation cannot be performed on a list", castedNode.rightNode)
            'End If

            ''Type
            'Select Case castedNode.op.type
            '    Case tokenType.OP_MINUS
            '        If left.TargetClass.compiledName = right.TargetClass.compiledName Then
            '            Return New safeType(stdInt)
            '        End If
            '        Return New safeType(stdFloat)

            '    Case tokenType.OP_PLUS
            '        If left.TargetClass.compiledName = "str" And right.TargetClass.compiledName = "str" Then
            '            Return New safeType(stdStr)
            '        ElseIf left.TargetClass.compiledName = "int" And right.TargetClass.compiledName = "int" Then
            '            Return New safeType(stdInt)
            '        End If
            '        Return New safeType(stdFloat)

            '    Case tokenType.OP_MULTIPLICATION
            '        If left.TargetClass.compiledName = "int" And right.TargetClass.compiledName = "int" Then
            '            Return New safeType(stdInt)
            '        ElseIf left.TargetClass.compiledName = "int" And right.TargetClass.compiledName = "str" Then
            '            Return New safeType(stdStr)
            '        ElseIf left.TargetClass.compiledName = "str" And right.TargetClass.compiledName = "int" Then
            '            Return New safeType(stdStr)
            '        End If
            '        Return New safeType(stdFloat)

            '    Case tokenType.OP_MODULO
            '        Return New safeType(stdInt)
            '        'TODO: Better modulo type

            '    Case tokenType.OP_DIVISION
            '        Return New safeType(stdFloat)
            '        'TODO: Better modulo type

            '    Case Else
            '        'Problem go brrr
            '        Throw New NotImplementedException()

            'End Select

        End If

        'StringNode
        If TypeOf node Is StringNode Then

            'Return
            Return New safeType(stdStr)

        End If

        'Boolean
        If TypeOf node Is BooleanNode Then

            'Return
            Return New safeType(stdBool)

        End If

        'BoolOp
        If TypeOf node Is boolOpNode Then

            'Return
            Return New safeType(stdBool)

        End If

        'ComparisonNode
        If TypeOf node Is ComparisonNode Then

            'Return
            Return New safeType(stdBool)

        End If

        'VariableNode
        If TypeOf node Is VariableNode Then

            'Castednode
            Dim castedNode As VariableNode = DirectCast(node, VariableNode)

            'Get variable
            Dim var As Variable = getVariable(castedNode.VariableName, castedNode)

            'Return
            Return var.type

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

        'BracketsSelectorNode
        If TypeOf node Is BracketsSelectorNode Then

            'Castednode
            Dim castedNode As BracketsSelectorNode = DirectCast(node, BracketsSelectorNode)

            'Get type
            Dim valueType As safeType = getNodeType(castedNode.Target)

            'Return
            Return valueType.getParentType()

        End If

        'ListNode
        If TypeOf node Is ListNode Then

            'Castednode
            Dim castedNode As ListNode = DirectCast(node, ListNode)

            'Handle no value
            If castedNode.elements.Count = 0 Then
                addNodeTypeError("VBGNT04", "A list cannot be empty, as this does not identify its type.", node)
            End If

            'Get type
            Dim type As safeType = getNodeType(castedNode.elements(0))
            type.Dimensions.Add(ValueType.list)

            'Get type
            Return type

        End If

        'MapNode
        If TypeOf node Is MapNode Then

            'Castednode
            Dim castedNode As MapNode = DirectCast(node, MapNode)

            'Handle no value
            If castedNode.elements.Count = 0 Then
                addNodeTypeError("VBGNT05", "A map cannot be empty, as this does not identify its type.", node)
            End If

            'Get type
            Dim type As safeType = getNodeType(castedNode.elements(0)(1))
            type.Dimensions.Add(ValueType.map)

            'Get type
            Return type

        End If

        'New node
        If TypeOf node Is newNode Then

            'Castednode
            Dim castedNode As newNode = DirectCast(node, newNode)

            'Get class
            Dim targetClass As ClassNode = getClass(castedNode.className, castedNode)

            'Get type
            Return New safeType(targetClass)

        End If

        'ChildNode
        If TypeOf node Is childNode Then

            'Castednode
            Dim castedNode As childNode = DirectCast(node, childNode)

            'Get class
            Dim parentType As safeType = getNodeType(castedNode.parentStruct)

            'Dimensions
            If parentType.Dimensions.Count > 0 Then

                If parentType.Dimensions(parentType.Dimensions.Count - 1) = ValueType.list Then

                    'LIST

                    'child type
                    If TypeOf castedNode.childNode Is VariableNode Then

                        'Variables
                        Dim propertieName As String = DirectCast(castedNode.childNode, VariableNode).VariableName

                        'Return type
                        Select Case propertieName
                            Case Else
                                addNodeNamingError("VBGNT07", "The <" & parentType.ToString() & "> list does not contain a """ & propertieName & """ propertie", castedNode.childNode)

                        End Select


                    ElseIf TypeOf castedNode.childNode Is FunctionCallNode Then

                        'FunctionCall
                        Dim funCall As FunctionCallNode = DirectCast(castedNode.childNode, FunctionCallNode)

                        'Return type
                        Select Case funCall.FunctionName
                            Case "len"
                                Return New safeType(stdInt)

                            Case "contains"
                                Return New safeType(stdBool)

                            Case "str"
                                Return New safeType(stdStr)

                            Case "clone"
                                Return parentType.clone()

                            Case "add"
                                addNodeSyntaxError("VBGNT09", "The ""add"" method returns no value.", castedNode.childNode)

                            Case "remove"
                                addNodeSyntaxError("VBGNT10", "The ""remove"" method returns no value.", castedNode.childNode)

                            Case "removeAt"
                                addNodeSyntaxError("VBGNT11", "The ""remove"" method returns no value.", castedNode.childNode)

                            Case Else
                                addNodeNamingError("VBGNT08", "The <" & parentType.ToString() & "> list does not contain a """ & funCall.FunctionName & """ method", castedNode.childNode)

                        End Select

                    End If

                ElseIf parentType.Dimensions(parentType.Dimensions.Count - 1) = ValueType.map Then

                    'MAP

                    'child type
                    If TypeOf castedNode.childNode Is VariableNode Then

                        'Variables
                        Dim propertieName As String = DirectCast(castedNode.childNode, VariableNode).VariableName

                        'Return type
                        Select Case propertieName
                            Case "keys"
                                Return New safeType(stdStr)

                            Case Else
                                addNodeNamingError("VBGNT12", "The <" & parentType.ToString() & "> map does not contain a """ & propertieName & """ propertie", castedNode.childNode)

                        End Select


                    ElseIf TypeOf castedNode.childNode Is FunctionCallNode Then

                        'FunctionCall
                        Dim funCall As FunctionCallNode = DirectCast(castedNode.childNode, FunctionCallNode)

                        'Return type
                        Select Case funCall.FunctionName
                            Case "len"
                                Return New safeType(stdInt)

                            Case "containsKey"
                                Return New safeType(stdBool)

                            Case "str"
                                Return New safeType(stdStr)

                            Case "clone"
                                Return parentType.clone()

                            Case "add"
                                addNodeSyntaxError("VBGNT13", "The ""add"" method returns no value.", castedNode.childNode)

                            Case "remove"
                                addNodeSyntaxError("VBGNT14", "The ""remove"" method returns no value.", castedNode.childNode)

                            Case "removeAt"
                                addNodeSyntaxError("VBGNT15", "The ""remove"" method returns no value.", castedNode.childNode)

                            Case Else
                                addNodeNamingError("VBGNT16", "The <" & parentType.ToString() & "> map does not contain a """ & funCall.FunctionName & """ method", castedNode.childNode)

                        End Select

                    End If

                Else
                    'Not implemented
                    Throw New NotImplementedException()

                End If


            End If

            'No dimensions
            If TypeOf castedNode.childNode Is VariableNode Then

                'Variables
                Dim searchName As String = DirectCast(castedNode.childNode, VariableNode).VariableName

                'Search propertie
                For Each var As Variable In parentType.TargetClass.variables
                    If var.name = searchName Then
                        Return var.type
                    End If
                Next

                'Not found
                addNodeNamingError("VBGNT06", "The <" & parentType.TargetClass.Name & "> class does not contain a """ & searchName & """ propertie", castedNode.childNode)

            ElseIf TypeOf castedNode.childNode Is FunctionCallNode Then

                'Variables
                Dim searchName As String = DirectCast(castedNode.childNode, FunctionCallNode).FunctionName

                'Search method
                For Each fun As FunctionNode In parentType.TargetClass.methods
                    If fun.Name = searchName Then
                        Return fun.ReturnType
                    End If
                Next

                'Not found
                addNodeNamingError("VBGNT06", "The <" & parentType.TargetClass.Name & "> class does not contain a """ & searchName & """ method", castedNode.childNode)

            End If

        End If

        'Return
        addNodeTypeError("VBGNT01", "Unable to resolve node type", node)
        Return Nothing

    End Function

    '==================================
    '========== COMPILE NODE ==========
    '==================================
    Private Function compileNode(ByVal node As Node, ByVal content As List(Of String)) As String

        If TypeOf node Is DeclareVariableNode Then

            'Compile
            Return compileDeclareVariable(DirectCast(node, DeclareVariableNode), content)

        ElseIf TypeOf node Is SetVariableNode Then

            'Compîle
            Return compileSetVariable(DirectCast(node, SetVariableNode), content)

        ElseIf TypeOf node Is FunctionCallNode Then

            'Compile
            Return compileFunctionCall(DirectCast(node, FunctionCallNode), content)

        ElseIf TypeOf node Is ReturnNode Then

            'Compile
            Return compileReturn(DirectCast(node, ReturnNode), content)

        ElseIf TypeOf node Is StringNode Then

            'Compile
            Return compileString(DirectCast(node, StringNode), content)

        ElseIf TypeOf node Is valueNode Then

            'Compile
            Return compileValue(DirectCast(node, valueNode), content)

        ElseIf TypeOf node Is BooleanNode Then

            'Compile
            Return compileBoolean(DirectCast(node, BooleanNode), content)

        ElseIf TypeOf node Is UnaryOpNode Then

            'Compile
            Return compileUnaryOp(DirectCast(node, UnaryOpNode), content)

        ElseIf TypeOf node Is binOpNode Then

            'Compile
            Return compileBinOp(DirectCast(node, binOpNode), content)

        ElseIf TypeOf node Is VariableNode Then

            'Compile
            Return compileVariable(DirectCast(node, VariableNode), content)

        ElseIf TypeOf node Is BracketsSelectorNode Then

            'Compile
            Return compileBracketsSelector(DirectCast(node, BracketsSelectorNode), content)

        ElseIf TypeOf node Is ListNode Then

            'Compile
            Return compileList(DirectCast(node, ListNode), content)

        ElseIf TypeOf node Is MapNode Then

            'Compile
            Return compileMap(DirectCast(node, MapNode), content)

        ElseIf TypeOf node Is ComparisonNode Then

            'Compile
            Return compileComparison(DirectCast(node, ComparisonNode), content)

        ElseIf TypeOf node Is newNode Then

            'Compile
            Return compileNew(DirectCast(node, newNode), content)

        ElseIf TypeOf node Is childNode Then

            'Compile
            Return compileChild(DirectCast(node, childNode), content)

        ElseIf TypeOf node Is AddSourceNode Then

            'Compile
            Return compileAddSource(DirectCast(node, AddSourceNode), content)

        ElseIf TypeOf node Is whileStatementNode Then

            'Compile
            Return compileWhileStatement(DirectCast(node, whileStatementNode), content)

        ElseIf TypeOf node Is forStatementNode Then

            'Compile
            Return compileForStatement(DirectCast(node, forStatementNode), content)

        ElseIf TypeOf node Is ifStatementNode Then

            'Compile
            Return compileIfStatement(DirectCast(node, ifStatementNode), content)

        ElseIf TypeOf node Is boolOpNode Then

            'Compile
            Return compileBoolOp(DirectCast(node, boolOpNode), content)

        End If

        'Return
        addNodeTypeError("VBCCN01", "Unable to resolve node type", node)
        Return Nothing

    End Function

    '=============================================
    '========== COMPILE WHILE STATEMENT ==========
    '=============================================
    Private Function compileWhileStatement(ByVal node As whileStatementNode, ByVal content As List(Of String))

        'Check type
        Dim conditionType As safeType = getNodeType(node.condition)
        If Not conditionType.IsTheSameAs(New safeType(stdBool)) Then
            addNodeTypeError("VBCWS01", "The condition of a while loop must be of type <bool>. However, you indicate a value of type <" & conditionType.ToString() & ">", node.condition)
        End If

        'Compile while header
        content.Add("")
        content.Add(String.Format("While {0}.value", compileNode(node.condition, content)))

        'Compile core
        Dim core As New List(Of String)
        For Each line As Node In node.codes
            core.Add(compileNode(line, core))
        Next
        For Each line As String In core
            content.Add(vbTab & line)
        Next

        'End
        content.Add("End While")
        content.Add("")

        'Return
        Return ""

    End Function

    '===========================================
    '========== COMPILE FOR STATEMENT ==========
    '===========================================
    Private Function compileForStatement(ByVal node As forStatementNode, ByVal content As List(Of String))

        'Check type
        Dim targetType As safeType = getNodeType(node.looperTarget)
        If Not targetType.Dimensions.Count > 0 Then
            addNodeTypeError("VBCFS01", "A ""for"" loop can only iterate through a list.", node.looperTarget)
        End If
        If Not targetType.Dimensions(targetType.Dimensions.Count - 1) = ValueType.list Then
            addNodeTypeError("VBCFS02", "A ""for"" loop can only iterate through a list.", node.looperTarget)
        End If

        'Create variable
        Dim compiledVariableName As String
        If getNodeParentFile(node).LimLib Then
            compiledVariableName = node.variableName
        Else
            compiledVariableName = getVariableName()
        End If
        Dim var As New Variable(node.variableName, getNodeType(node.looperTarget).getParentType(), compiledVariableName, node.variableDeclareType)
        node.variables.Add(var)

        'Compile while header
        content.Add("")
        content.Add(String.Format("For Each {0} As {1} In {2}", var.compiledName, compileSafeType(var.type), compileNode(node.looperTarget, content)))

        'Clone
        If var.declarationType = VariableDeclarationType._var_ Then
            content.Add(vbTab & String.Format("{0} = {0}.clone()", var.compiledName))
        End If

        'Compile core
        Dim core As New List(Of String)
        For Each line As Node In node.codes
            core.Add(compileNode(line, core))
        Next
        For Each line As String In core
            content.Add(vbTab & line)
        Next

        'End
        content.Add("Next")
        content.Add("")

        'Return
        Return ""

    End Function

    '==========================================
    '========== COMPILE IF STATEMENT ==========
    '==========================================
    Private Function compileIfStatement(ByVal node As ifStatementNode, ByVal content As List(Of String))

        'Check type
        Dim if_condition_type As safeType = getNodeType(node.condition)
        If Not if_condition_type.IsTheSameAs(New safeType(stdBool)) Then
            addNodeTypeError("VBCIS01", "An ""if"" block needs a condition", node.condition)
        End If

        'Variables
        Dim headerContent As New List(Of String)
        Dim tempContent As New List(Of String)

        'Compile while header
        tempContent.Add("")
        tempContent.Add(String.Format("If {0}.value Then", compileNode(node.condition, headerContent)))

        'Compile if core
        Dim core As New List(Of String)
        For Each line As Node In node.if_statements
            core.Add(compileNode(line, core))
        Next
        For Each line As String In core
            tempContent.Add(vbTab & line)
        Next

        'Else if
        For Each elseif_statement As Tuple(Of Node, List(Of Node)) In node.elseif_statements

            'Check type
            Dim elseif_condition_type As safeType = getNodeType(elseif_statement.Item1)
            If Not elseif_condition_type.IsTheSameAs(New safeType(stdBool)) Then
                addNodeTypeError("VBCIS01", "An ""elseif"" block needs a condition", elseif_statement.Item1)
            End If

            'Compile
            tempContent.Add(String.Format("ElseIf {0}.value Then", compileNode(elseif_statement.Item1, headerContent)))

            'Compile if core
            core.Clear()
            For Each line As Node In elseif_statement.Item2
                core.Add(compileNode(line, core))
            Next
            For Each line As String In core
                tempContent.Add(vbTab & line)
            Next

        Next

        'Else
        If node.else_statement.Count > 0 Then

            'Compile
            tempContent.Add("Else")

            'Compile if core
            core.Clear()
            For Each line As Node In node.else_statement
                core.Add(compileNode(line, core))
            Next
            For Each line As String In core
                tempContent.Add(vbTab & line)
            Next

        End If

        'End
        tempContent.Add("End If")
        tempContent.Add("")

        'Fix
        For Each line As String In headerContent
            content.Add(line)
        Next
        For Each line As String In tempContent
            content.Add(line)
        Next

        'Return
        Return ""

    End Function

    '========================================
    '========== COMPILE ADD SOURCE ==========
    '========================================
    Private Function compileAddSource(ByVal node As AddSourceNode, ByVal content As List(Of String))

        'Check if file is limlib
        If getNodeParentFile(node).LimLib Then

            'Compile
            content.Add(DirectCast(node, AddSourceNode).value)

        Else

            'Error
            addNodeSyntaxError("VBCCN02", "It Is Not possible To integrate source code outside Of a ""limlib"" file.", node)

        End If

        'Return
        Return ""

    End Function

    '===================================
    '========== COMPILE CHILD ==========
    '===================================
    Private Function compileChild(ByVal node As childNode, ByVal content As List(Of String))

        'Get class
        Dim parentType As safeType = getNodeType(node.parentStruct)

        'Dimensions
        If parentType.Dimensions.Count > 0 Then

            If parentType.Dimensions(parentType.Dimensions.Count - 1) = ValueType.list Then

                'LIST

                'child type
                If TypeOf node.childNode Is VariableNode Then

                    'Variables
                    Dim propertieName As String = DirectCast(node.childNode, VariableNode).VariableName

                    'Return type
                    Select Case propertieName
                        Case Else
                            addNodeNamingError("VBGNT07", "The <" & parentType.ToString() & "> list does Not contain a """ & propertieName & """ propertie", node.childNode)

                    End Select


                ElseIf TypeOf node.childNode Is FunctionCallNode Then

                    'FunctionCall
                    Dim funCall As FunctionCallNode = DirectCast(node.childNode, FunctionCallNode)

                    'Return type
                    Select Case funCall.FunctionName
                        Case "len"
                            validateChildFunction(Nothing, funCall.Arguments, funCall)
                            Return String.Format("(New int({0}.Count))", compileNode(node.parentStruct, content))

                        Case "contains"
                            validateChildFunction({parentType.getParentType()}.ToList(), funCall.Arguments, funCall)
                            Return String.Format("(New bool({0}.Contains({1})))", compileNode(node.parentStruct, content), compileNode(funCall.Arguments(0), content))

                        Case "str"
                            validateChildFunction(Nothing, funCall.Arguments, funCall)
                            Return String.Format("({0}.__str__())", compileNode(node.parentStruct, content))

                        Case "clone"
                            validateChildFunction(Nothing, funCall.Arguments, funCall)
                            Return String.Format("({0}.clone())", compileNode(node.parentStruct, content))

                        Case "add"
                            validateChildFunction({parentType.getParentType()}.ToList(), funCall.Arguments, funCall)
                            content.Add("")
                            content.Add(String.Format("{0}.add({1})", compileNode(node.parentStruct, content), compileNode(funCall.Arguments(0), content)))
                            content.Add("")
                            Return ""

                        Case "remove"

                        Case "removeAt"

                        Case Else
                            addNodeNamingError("VBCC09", "The <" & parentType.ToString() & "> list does Not contain a """ & funCall.FunctionName & """ method", node.childNode)

                    End Select

                End If

            ElseIf parentType.Dimensions(parentType.Dimensions.Count - 1) = ValueType.map Then

                'MAP

                'child type
                If TypeOf node.childNode Is VariableNode Then

                    'Variables
                    Dim propertieName As String = DirectCast(node.childNode, VariableNode).VariableName

                    'Return type
                    Select Case propertieName
                        Case "keys"
                            Return New safeType(stdStr)

                        Case Else
                            addNodeNamingError("VBCC10", "The <" & parentType.ToString() & "> map does Not contain a """ & propertieName & """ propertie", node.childNode)

                    End Select


                ElseIf TypeOf node.childNode Is FunctionCallNode Then

                    'FunctionCall
                    Dim funCall As FunctionCallNode = DirectCast(node.childNode, FunctionCallNode)

                    'Return type
                    Select Case funCall.FunctionName
                        Case "len"
                            validateChildFunction(Nothing, funCall.Arguments, funCall)
                            Return String.Format("(New int({0}.Count))", compileNode(node.parentStruct, content))

                        Case "containsKey"


                        Case "str"
                            validateChildFunction(Nothing, funCall.Arguments, funCall)
                            Return String.Format("({0}.__str__())", compileNode(node.parentStruct, content))

                        Case "clone"
                            validateChildFunction(Nothing, funCall.Arguments, funCall)
                            Return String.Format("({0}.clone())", compileNode(node.parentStruct, content))

                        Case "add"
                            addNodeSyntaxError("VBCC11", "The ""add"" method returns no value.", node.childNode)

                        Case "remove"
                            addNodeSyntaxError("VBCC12", "The ""remove"" method returns no value.", node.childNode)

                        Case "removeAt"
                            addNodeSyntaxError("VBCC13", "The ""remove"" method returns no value.", node.childNode)

                        Case Else
                            addNodeNamingError("VBCC14", "The <" & parentType.ToString() & "> map does Not contain a """ & funCall.FunctionName & """ method", node.childNode)

                    End Select

                End If

            Else
                'Not implemented
                Throw New NotImplementedException()

            End If

        Else

            'No dimensions
            If TypeOf node.childNode Is VariableNode Then

                'Variables
                Dim searchName As String = DirectCast(node.childNode, VariableNode).VariableName

                'Search propertie
                For Each var As Variable In parentType.TargetClass.variables
                    If var.name = searchName Then
                        Return String.Format("{0}.{1}", compileNode(node.parentStruct, content), var.compiledName)
                    End If
                Next

                'Not found
                addNodeNamingError("VBCC01", "The <" & parentType.TargetClass.Name & "> Class does Not contain a """ & searchName & """ propertie", node.childNode)

            ElseIf TypeOf node.childNode Is FunctionCallNode Then

                'Variables
                Dim funCall As FunctionCallNode = DirectCast(node.childNode, FunctionCallNode)
                Dim searchName As String = funCall.FunctionName

                'Search method
                For Each fun As FunctionNode In parentType.TargetClass.methods
                    If fun.Name = searchName Then

                        'Handle argument error
                        If funCall.Arguments.Count < fun.minArguments Then
                            addNodeTypeError("VBCC02", (fun.Arguments.Count - funCall.Arguments.Count).ToString() & " arguments are missing", node)
                        End If
                        If funCall.Arguments.Count > fun.maxArguments Then
                            addNodeTypeError("VBCC03", (funCall.Arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
                        End If

                        'Argument
                        Dim arguments As String = ""
                        For i As Integer = 0 To funCall.Arguments.Count - 1

                            'Variables
                            Dim argModel As FunctionArgument = fun.Arguments(i)
                            Dim argNode As Node = funCall.Arguments(i)

                            'Handle type error
                            If Not (typenodeToSafeType(argModel.type).IsTheSameAs(getNodeType(argNode))) Then
                                addNodeTypeError("VBCC04", "The " & (i + 1).ToString() & " argument Is Of type <" & getNodeType(argNode).ToString() & "> instead Of being <" & argModel.type.ToString() & ">", argNode)
                            End If

                            'Add node
                            Select Case argModel.declareType
                                Case VariableDeclarationType._let_
                                    arguments &= ", " & compileNode(argNode, content)

                                Case VariableDeclarationType._var_
                                    arguments &= ", " & compileNode(argNode, content) & ".clone()"

                                Case Else
                                    Throw New NotImplementedException

                            End Select

                        Next
                        If arguments.StartsWith(", ") Then
                            arguments = arguments.Substring(2)
                        End If

                        Return String.Format("{0}.{1}({2})", compileNode(node.parentStruct, content), fun.compiledName, arguments)

                    End If
                Next

                'Not found
                addNodeNamingError("VBCC05", "The <" & parentType.TargetClass.Name & "> Class does Not contain a """ & searchName & """ method", node.childNode)

            End If

        End If

        'Return
        Return ""

    End Function
    Private Sub validateChildFunction(ByVal model_arguments As List(Of safeType), ByVal passed_arguments As List(Of Node), ByVal from As Node)

        'Variable
        If model_arguments Is Nothing Then
            model_arguments = New List(Of safeType)
        End If

        'Handle argument error
        If passed_arguments.Count < model_arguments.Count Then
            addNodeTypeError("VBVCF01", (model_arguments.Count - passed_arguments.Count).ToString() & " arguments are missing", from)
        End If
        If passed_arguments.Count > model_arguments.Count Then
            addNodeTypeError("VBVCF02", (passed_arguments.Count - model_arguments.Count).ToString() & " arguments are useless (too many arguments)", from)
        End If

        'Argument
        Dim arguments As String = ""
        For i As Integer = 0 To model_arguments.Count - 1

            'Variables
            Dim argType As safeType = getNodeType(passed_arguments(i))

            If Not model_arguments(i).IsTheSameAs(argType) Then
                addNodeTypeError("VBVCF03", "The " & (i + 1).ToString() & " argument Is Of type <" & argType.ToString() & "> instead Of being <" & model_arguments(i).ToString() & ">", passed_arguments(i))
            End If

        Next

    End Sub

    '======================================
    '========== COMPILE VARIABLE ==========
    '======================================
    Public Function compileVariable(ByVal node As VariableNode, ByVal content As List(Of String)) As String

        'Get variable
        Dim var As Variable = getVariable(node.VariableName, node)

        'Clone
        If var.type.TargetClass.primary Then
            Return var.compiledName & ".clone()"
        Else
            Return var.compiledName
        End If

    End Function

    '====================================
    '========== COMPILE STRING ==========
    '====================================
    Public Function compileString(ByVal node As StringNode, ByVal content As List(Of String)) As String

        'Return
        Return "New str(""" & node.value.ToString() & """)"

    End Function

    '===================================
    '========== COMPILE VALUE ==========
    '===================================
    Public Function compileValue(ByVal node As valueNode, ByVal content As List(Of String)) As String

        'Return
        Select Case node.tok.type

            Case tokenType.CT_FLOAT
                Return "New float(" & node.tok.value.ToString() & ")"

            Case tokenType.CT_INTEGER
                Return "New int(" & node.tok.value.ToString() & ")"

            Case Else
                Throw New NotImplementedException

        End Select

    End Function

    '=====================================
    '========== COMPILE BOOLEAN ==========
    '=====================================
    Public Function compileBoolean(ByVal node As BooleanNode, ByVal content As List(Of String)) As String

        'Return
        If node.value Then
            Return "New bool(True)"
        Else
            Return "New bool(False)"
        End If

    End Function

    '======================================
    '========== COMPILE UNARY OP ==========
    '======================================
    Public Function compileUnaryOp(ByVal node As UnaryOpNode, ByVal content As List(Of String)) As String

        'Get value type
        Dim valueType As safeType = getNodeType(node.node)
        If valueType.Dimensions.Count > 0 Then
            addNodeTypeError("VBCUO02", "The """ & node.op.ToString() & """ Operator cannot be applied To a <" & valueType.ToString() & ">", node)
        End If

        'Return
        Select Case node.op.type

            Case tokenType.OP_PLUS
                If valueType.TargetClass.compiledName = "int" Then
                    Return "New int(+(" & compileNode(node.node, content) & ".value))"
                ElseIf valueType.TargetClass.compiledName = "float" Then
                    Return "New float(+(" & compileNode(node.node, content) & ".value))"
                End If

            Case tokenType.OP_MINUS
                If valueType.TargetClass.compiledName = "int" Then
                    Return "New int(-(" & compileNode(node.node, content) & ".value))"
                ElseIf valueType.TargetClass.compiledName = "float" Then
                    Return "New float(-(" & compileNode(node.node, content) & ".value))"
                End If

            Case Else
                Throw New NotImplementedException

        End Select

        'Error
        addNodeTypeError("VBCUO01", "The """ & node.op.ToString() & """ Operator cannot be applied To a <" & valueType.ToString() & ">", node)
        Return Nothing

    End Function

    '===============================================
    '========== COMPILE BRACKETS SELECTOR ==========
    '===============================================
    Public Function compileBracketsSelector(ByVal node As BracketsSelectorNode, ByVal content As List(Of String)) As String

        'Variables
        Dim targetType As safeType = getNodeType(node.Target)

        'Handle error
        If Not targetType.Dimensions.Count > 0 Then
            addNodeTypeError("VBCBS01", "Chosen item must be at least map Or list To Select index/key", node.Target)
        End If
        If Not {ValueType.list, ValueType.map}.Contains(targetType.Dimensions(targetType.Dimensions.Count - 1)) Then
            addNodeTypeError("VBCBS02", "Chosen item must be at least map Or list To Select index/key", node.Target)
        End If

        'Get type
        Dim indexType As safeType = getNodeType(node.index)

        'Compile
        Select Case targetType.Dimensions(targetType.Dimensions.Count - 1)
            Case ValueType.list
                If Not (indexType.TargetClass.compiledName = "int" And indexType.Dimensions.Count = 0) Then
                    addNodeTypeError("VBCBS03", "An index Of a list must be an Integer (<int>)", node.index)
                End If
                Return String.Format("{0}.getAt({1}.value)", compileNode(node.Target, content), compileNode(node.index, content))

            Case ValueType.map
                If Not (indexType.TargetClass.compiledName = "str" And indexType.Dimensions.Count = 0) Then
                    addNodeTypeError("VBCBS04", "An key name Of a map must be an String (<str>)", node.index)
                End If
                Return String.Format("{0}.getBy({1}.value)", compileNode(node.Target, content), compileNode(node.index, content))

        End Select

        'Error
        Return ""

    End Function

    '====================================
    '========== COMPILE BIN OP ==========
    '====================================
    Public Function compileBinOp(ByVal node As binOpNode, ByVal content As List(Of String)) As String

        'Get value type
        Dim leftType As safeType = getNodeType(node.leftNode)
        Dim rightType As safeType = getNodeType(node.rightNode)

        'Casted node
        Dim castedNode As binOpNode = DirectCast(node, binOpNode)

        'Get type
        Dim left As safeType = getNodeType(castedNode.leftNode)
        Dim right As safeType = getNodeType(castedNode.rightNode)

        'Get relation
        For Each relation As RelationNode In left.TargetClass.relations

            'Get argument type
            Dim relationRightArgumentType As safeType = typenodeToSafeType(relation.Arguments(1).type)

            'If it's not the same
            If Not relationRightArgumentType.IsTheSameAs(right) Then
                Continue For
            End If
            If Not relation.operator_name.type = castedNode.op.type Then
                Continue For
            End If

            'Compile
            Dim string_operator As String = ""
            Select Case castedNode.op.type

                Case tokenType.OP_PLUS
                    string_operator = "+"

                Case tokenType.OP_MINUS
                    string_operator = "-"

                Case tokenType.OP_MULTIPLICATION
                    string_operator = "*"

                Case tokenType.OP_DIVISION
                    string_operator = "/"

                Case tokenType.OP_MODULO
                    string_operator = "Mod"

                Case Else
                    addSyntaxError("VBCBO02", "The compiler does not recognize this operator despite the fact that it is defined from a relation.", getNodeParentFile(castedNode), castedNode.op.positionStart, castedNode.op.positionEnd)

            End Select
            Return "(" & compileNode(castedNode.leftNode, content) & " " & string_operator & " " & compileNode(castedNode.rightNode, content) & ")"
        Next

        'Error
        addNodeTypeError("VBCBO01", "The operation between an <" & left.ToString() & "> type and a <" & right.ToString() & "> type is undefined.", castedNode, "Create a ""relation"" of these two types in the class <" & left.TargetClass.Name & ">")
        Return Nothing

        ''Return
        'Select Case node.op.type

        '    Case tokenType.OP_PLUS
        '        'ADDITIONS

        '        'Check error
        '        If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
        '            addNodeTypeError("VBCBO01", "Both elements cannot be a list To allow a ""+"" operation.", node)
        '        End If

        '        'Number operation
        '        If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "int" Then
        '            Return String.Format("New int({0}.value + {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If
        '        If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
        '            Return String.Format("New float({0}.value + {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If

        '        'String operation
        '        If leftType.TargetClass.compiledName = "str" And rightType.TargetClass.compiledName = "str" Then
        '            Return String.Format("New str({0}.value & {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If

        '    Case tokenType.OP_MINUS
        '        'SUBSTRACTION

        '        'Check error
        '        If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
        '            addNodeTypeError("VBCBO02", "Both elements cannot be a list To allow a ""-"" operation.", node)
        '        End If

        '        'Number operation
        '        If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "int" Then
        '            Return String.Format("New int({0}.value - {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If
        '        If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
        '            Return String.Format("New float({0}.value - {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If

        '    Case tokenType.OP_MULTIPLICATION
        '        'MULTIPLICATION

        '        'Check error
        '        If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
        '            addNodeTypeError("VBCBO03", "Both elements cannot be a list To allow a ""*"" operation.", node)
        '        End If

        '        'Number operation
        '        If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "int" Then
        '            Return String.Format("New int({0}.value * {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If
        '        If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
        '            Return String.Format("New float({0}.value * {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If

        '        'String & number
        '        If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "str" Then
        '            Dim helpVar As String = getHelpVariableName()
        '            Dim iVar As String = getHelpVariableName()
        '            content.Add(String.Format("Dim {0} As String = """"", helpVar))
        '            content.Add(String.Format("For {0} As Integer = 0 To {1}.value - 1", iVar, compileNode(node.leftNode, content)))
        '            content.Add(String.Format("{0}{1} &= {2}.value", vbTab, helpVar, compileNode(node.rightNode, content)))
        '            content.Add("Next")
        '            Return String.Format("New str({0})", helpVar)
        '        End If

        '        'Number & String
        '        If leftType.TargetClass.compiledName = "str" And rightType.TargetClass.compiledName = "int" Then
        '            Dim helpVar As String = getHelpVariableName()
        '            Dim iVar As String = getHelpVariableName()
        '            content.Add(String.Format("Dim {0} As String = """"", helpVar))
        '            content.Add(String.Format("For {0} As Integer = 0 To {1}.value - 1", iVar, compileNode(node.rightNode, content)))
        '            content.Add(String.Format("{0}{1} &= {2}.value", vbTab, helpVar, compileNode(node.leftNode, content)))
        '            content.Add("Next")
        '            Return String.Format("New str({0})", helpVar)
        '        End If

        '    Case tokenType.OP_DIVISION
        '        'DIVISION

        '        'Check error
        '        If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
        '            addNodeTypeError("VBCBO04", "Both elements cannot be a list To allow a ""/"" operation.", node)
        '        End If

        '        'Number operation
        '        If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
        '            Return String.Format("New float({0}.value / {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
        '        End If

        'End Select

        ''Error
        'addNodeTypeError("VBCBO01", "The """ & node.op.ToString() & """ operation between a member of type <" & leftType.ToString() & "> and <" & rightType.ToString() & "> is not possible.", node)
        'Return Nothing

    End Function

    '=====================================
    '========== COMPILE BOOL OP ==========
    '=====================================
    Public Function compileBoolOp(ByVal node As boolOpNode, ByVal content As List(Of String)) As String

        'Get value type
        Dim leftType As safeType = getNodeType(node.leftNode)
        Dim rightType As safeType = getNodeType(node.rightNode)

        'Handle error
        If leftType.Dimensions.Count > 0 Then
            addNodeTypeError("VBCBOO01", "Left elements cannot be a list or a map To allow a boolean operation.", node.leftNode)
        End If
        If rightType.Dimensions.Count > 0 Then
            addNodeTypeError("VBCBOO02", "Right elements cannot be a list or a map To allow a boolean operation.", node.rightNode)
        End If
        If Not leftType.TargetClass.compiledName = "bool" Then
            addNodeTypeError("VBCBOO03", "The element is of type <" & leftType.ToString() & ">, it should be of type <bool> to allow boolean comparison.", node.leftNode)
        End If
        If Not rightType.TargetClass.compiledName = "bool" Then
            addNodeTypeError("VBCBOO04", "The element is of type <" & rightType.ToString() & ">, it should be of type <bool> to allow boolean comparison.", node.rightNode)
        End If

        'Return
        Select Case node.op.type

            Case tokenType.OP_AND
                'AND
                Return String.Format("New bool({0}.value And {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))

            Case tokenType.OP_OR
                'OR
                Return String.Format("New bool({0}.value Or {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))

        End Select

        'Error
        addNodeTypeError("VBCBO01", "The """ & node.op.ToString() & """ operation between a member of type <" & leftType.ToString() & "> and <" & rightType.ToString() & "> is not possible.", node)
        Return Nothing

    End Function

    '========================================
    '========== COMPILE COMPARISON ==========
    '========================================
    Public Function compileComparison(ByVal node As ComparisonNode, ByVal content As List(Of String)) As String

        'Get value type
        Dim leftType As safeType = getNodeType(node.leftNode)
        Dim rightType As safeType = getNodeType(node.rightNode)

        'Return
        Select Case node.op.type

            Case tokenType.OP_EQUAL
                'EQUAL

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCC01", "Both elements cannot be a list to allow a ""="" comparison.", node)
                End If

                'Number operation
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New bool({0}.value = {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

                'String operation
                If leftType.TargetClass.compiledName = "str" And rightType.TargetClass.compiledName = "str" Then
                    Return String.Format("New bool({0}.value = {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

                'Bool operation
                If leftType.TargetClass.compiledName = "bool" And rightType.TargetClass.compiledName = "bool" Then
                    Return String.Format("New bool({0}.value = {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_MORETHAN
                'MORE THAN

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCC02", "Both elements cannot be a list to allow a "">"" comparison.", node)
                End If

                'Number operation
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New bool({0}.value > {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_MORETHANEQUAL
                'MORE THAN EQUAL

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCC03", "Both elements cannot be a list to allow a "">="" comparison.", node)
                End If

                'Number operation
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New bool({0}.value >= {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_LESSTHAN
                'LESS THAN

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCC04", "Both elements cannot be a list to allow a ""<"" comparison.", node)
                End If

                'Number operation
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New bool({0}.value < {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_LESSTHANEQUAL
                'LESS THAN EQUAL

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCC05", "Both elements cannot be a list to allow a ""<="" comparison.", node)
                End If

                'Number operation
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New bool({0}.value <= {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_IN
                Throw New NotImplementedException()

        End Select

        'Error
        addNodeTypeError("VBCC06", "The """ & node.op.ToString() & """ comparison between a member of type <" & leftType.ToString() & "> and <" & rightType.ToString() & "> is not possible.", node)
        Return Nothing

    End Function

    '==================================
    '========== COMPILE LIST ==========
    '==================================
    Public Function compileList(ByVal node As ListNode, ByVal content As List(Of String)) As String

        'Handle no value
        If node.elements.Count = 0 Then
            addNodeTypeError("VBCL01", "A list cannot be empty, as this does not identify its type.", node)
        End If

        'Get type
        Dim listType As safeType = getNodeType(node.elements(0))

        'Compile each arguments
        Dim helpVar As String = getHelpVariableName()
        content.Add(String.Format("Dim {0} As New LimList(Of {1})", helpVar, compileSafeType(listType)))
        For Each elm As Node In node.elements

            'Handle type error
            Dim elmType As safeType = getNodeType(elm)
            If Not listType.IsTheSameAs(elmType) Then
                addNodeTypeError("VBCL02", "The following element <" & elmType.ToString() & "> is not of the type of the list <" & listType.ToString() & ">", elm)
            End If

            'Compile
            content.Add(String.Format("{0}.Add({1})", helpVar, compileNode(elm, content)))

        Next

        'Finish compile
        Return helpVar

    End Function

    '=================================
    '========== COMPILE MAP ==========
    '=================================
    Public Function compileMap(ByVal node As MapNode, ByVal content As List(Of String)) As String

        'Handle no value
        If node.elements.Count = 0 Then
            addNodeTypeError("VBCM01", "A list cannot be empty, as this does not identify its type.", node)
        End If

        'Get type
        Dim valueType As safeType = getNodeType(node.elements(0)(1))
        Dim helpVar As String = getHelpVariableName()
        content.Add(String.Format("Dim {0} As New LimMap(Of {1})", helpVar, valueType))

        'Compile each arguments
        For Each elm As List(Of Node) In node.elements

            'Handle type error
            Dim elmType As safeType = getNodeType(elm(1))
            If Not valueType.IsTheSameAs(elmType) Then
                addNodeTypeError("VBCM02", "The following element <" & elmType.ToString() & "> is not of the type of the map <" & valueType.ToString() & ">", elm(1))
            End If
            Dim keyType As safeType = getNodeType(elm(0))
            If Not (keyType.Dimensions.Count = 0 And keyType.TargetClass.compiledName = "str") Then
                addNodeTypeError("VBCM03", "The key must be of type <str>, but is of type <" & keyType.ToString() & ">", elm(0))
            End If

            'Compile
            content.Add(String.Format("{0}.TryAdd({1}.value, {2})", helpVar, compileNode(elm(0), content), compileNode(elm(1), content)))

        Next

        'Finish compile
        Return helpVar

    End Function

    '=================================
    '========== COMPILE NEW ==========
    '=================================
    Private Function compileNew(ByVal node As newNode, ByVal content As List(Of String)) As String

        'Get class
        Dim targetClass As ClassNode = getClass(node.className, node)

        'Get constructor
        Dim fun As FunctionNode = Nothing
        For Each method As FunctionNode In targetClass.methods
            If method.Name = "create" Then
                fun = method
            End If
        Next
        If fun Is Nothing Then
            If node.arguments.Count = 0 Then
                Return String.Format("(New {0}())", targetClass.compiledName)
            Else
                addNodeSyntaxError("VBCCN01", "Class <" & targetClass.Name & "> does not contain a constructor", node, "Add a ""create"" method to the class")
            End If
        End If

        'Handle argument error
        If node.arguments.Count < fun.minArguments Then
            addNodeSyntaxError("VBCCN02", (fun.Arguments.Count - node.arguments.Count).ToString() & " arguments are missing", node)
        End If
        If node.arguments.Count > fun.maxArguments Then
            addNodeSyntaxError("VBCCN03", (node.arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
        End If

        'Argument
        Dim arguments As String = ""
        For i As Integer = 0 To fun.Arguments.Count - 1

            'Variables
            Dim argModel As FunctionArgument = fun.Arguments(i)
            Dim argNode As Node = node.arguments(i)

            'Handle type error
            If Not (typenodeToSafeType(argModel.type).IsTheSameAs(getNodeType(argNode))) Then
                addNodeTypeError("VBCCN04", "The " & (i + 1).ToString() & " argument is of type <" & getNodeType(argNode).ToString() & "> instead of being <" & argModel.type.ToString() & ">", argNode)
            End If

            'Add node
            Select Case argModel.declareType
                Case VariableDeclarationType._let_
                    arguments &= ", " & compileNode(argNode, content)

                Case VariableDeclarationType._var_
                    arguments &= ", " & compileNode(argNode, content) & ".clone()"

                Case Else
                    Throw New NotImplementedException

            End Select

        Next
        If arguments.StartsWith(", ") Then
            arguments = arguments.Substring(2)
        End If

        'Return
        Return String.Format("(New {0}({1}))", targetClass.compiledName, arguments)

    End Function

    '===========================================
    '========== COMPILE FUNCTION CALL ==========
    '===========================================
    Private Function compileFunctionCall(ByVal node As FunctionCallNode, ByVal content As List(Of String)) As String

        'Get function
        Dim fun As FunctionNode = getFunction(node.FunctionName, node)
        'Handle argument error
        If node.Arguments.Count < fun.minArguments Then
            addNodeTypeError("VBCFC01", (fun.Arguments.Count - node.Arguments.Count).ToString() & " arguments are missing", node)
        End If
        If node.Arguments.Count > fun.maxArguments Then
            addNodeTypeError("VBCFC02", (node.Arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
        End If

        'Argument
        Dim arguments As String = ""
        For i As Integer = 0 To node.Arguments.Count - 1

            'Variables
            Dim argModel As FunctionArgument = fun.Arguments(i)
            Dim argNode As Node = node.Arguments(i)

            'Handle type error
            If Not (typenodeToSafeType(argModel.type).IsTheSameAs(getNodeType(argNode))) Then
                addNodeTypeError("VBCFC03", "The " & (i + 1).ToString() & " argument is of type <" & getNodeType(argNode).ToString() & "> instead of being <" & argModel.type.ToString() & ">", argNode)
            End If

            'Add node
            Select Case argModel.declareType
                Case VariableDeclarationType._let_
                    arguments &= ", " & compileNode(argNode, content)

                Case VariableDeclarationType._var_
                    arguments &= ", " & compileNode(argNode, content) & ".clone()"

                Case Else
                    Throw New NotImplementedException

            End Select

        Next
        If arguments.StartsWith(", ") Then
            arguments = arguments.Substring(2)
        End If

        'Return
        Return fun.compiledName & "(" & arguments & ")"

    End Function

    '==========================================
    '========== COMPILE SET VARIABLE ==========
    '==========================================
    Private Function compileSetVariable(ByVal node As SetVariableNode, ByVal content As List(Of String)) As String

        'Set variable
        Dim castedNode As SetVariableNode = DirectCast(node, SetVariableNode)

        'Get type
        Dim variableType As safeType = getNodeType(castedNode.Target)
        Dim valueType As safeType = getNodeType(castedNode.NewValue)

        'Handle error
        If Not variableType.IsTheSameAs(valueType) Then
            addNodeTypeError("VBCSV01", "It is impossible for a variable of type <" & variableType.ToString() & "> to assign itself a value of type <" & valueType.ToString() & ">", castedNode.NewValue)
        End If

        'Get variable
        Dim var As Variable = Nothing
        If TypeOf castedNode.Target Is VariableNode Then
            var = getVariable(DirectCast(castedNode.Target, VariableNode).VariableName, castedNode.Target)
            If var.declarationType = VariableDeclarationType._let_ Then
                content.Add(String.Format("{0} = {1}", var.compiledName, compileNode(castedNode.NewValue, content)))
            Else
                content.Add(String.Format("{0} = {1}.Clone()", compileNode(castedNode.Target, content), compileNode(castedNode.NewValue, content)))
            End If

        ElseIf TypeOf castedNode.Target Is BracketsSelectorNode Then
            'content.Add(String.Format("{0} = {1}", compileNode(castedNode.Target, content), compileNode(castedNode.NewValue, content)))

        ElseIf TypeOf castedNode.Target Is childNode Then
            content.Add(String.Format("{0} = {1}", compileNode(castedNode.Target, content), compileNode(castedNode.NewValue, content)))

        Else
            addNodeSyntaxError("VBCSV02", "It is not possible to assign a value to this target", castedNode.Target)

        End If

        'Return
        Return ""

    End Function

    '==============================================
    '========== COMPILE DECLARE VARIABLE ==========
    '==============================================
    Private Function compileDeclareVariable(ByVal node As DeclareVariableNode, ByVal content As List(Of String)) As String

        'Declare variable
        content.Add("")
        content.Add(String.Format("'{0}", node.variableName))

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
            var.type = typenodeToSafeType(node.variableUnsafeType)

            'List or something
            If var.type.Dimensions.Count > 0 Then
                'New
                content.Add(String.Format("Dim {0} As New {1}", var.compiledName, compileSafeType(var.type)))
            Else
                'Normal
                content.Add(String.Format("Dim {0} As {1} = Nothing", var.compiledName, compileSafeType(var.type)))
            End If

        Else

            'Set type
            var.type = getNodeType(node.value)
            If Not node.variableUnsafeType Is Nothing Then
                Dim defType As safeType = typenodeToSafeType(node.variableUnsafeType)
                If Not var.type.IsTheSameAs(defType) Then
                    addNodeTypeError("VBCCF02", "The defined type of the variable is not the same as that of its value.", node.value)
                End If
            End If

            'Ref
            Select Case var.declarationType

                Case VariableDeclarationType._let_
                    content.Add(String.Format("Dim {0} As {1} = {2}", var.compiledName, compileSafeType(var.type), compileNode(node.value, content)))

                Case VariableDeclarationType._var_
                    content.Add(String.Format("Dim {0} As {1} = {2}.clone()", var.compiledName, compileSafeType(var.type), compileNode(node.value, content)))

                Case Else
                    Throw New NotImplementedException

            End Select

        End If

        'Add variable
        getNodeParentContainer(node).variables.Add(var)

        'Return
        content.Add("")
        Return ""

    End Function

    '====================================
    '========== COMPILE RETURN ==========
    '====================================
    Private Function compileReturn(ByVal node As ReturnNode, ByVal content As List(Of String)) As String

        'Variables
        Dim valueType As safeType = getNodeType(node.value)

        'Function
        Dim func As FunctionNode = getNodeParentFunction(node, False)
        If func IsNot Nothing Then

            'Handle type error
            If Not func.ReturnType Is Nothing Then

                'Existing return type
                If Not func.ReturnType.IsTheSameAs(valueType) Then
                    addNodeTypeError("VBCR01", "The function returns a value of type <" & func.ReturnType.ToString() & ">, but here you return a value of type <" & valueType.ToString() & ">", node.value)
                End If

            Else

                'Set return type
                func.ReturnType = valueType

            End If

        End If

        'Relation
        Dim relation As RelationNode = getNodeParentRelation(node, False)
        If relation IsNot Nothing Then

            'Handle type error
            If Not relation.ReturnType Is Nothing Then

                'Existing return type
                If Not relation.ReturnType.IsTheSameAs(valueType) Then
                    addNodeTypeError("VBCR02", "The relation returns a value of type <" & relation.ReturnType.ToString() & ">, but here you return a value of type <" & valueType.ToString() & ">", node.value)
                End If

            Else

                'Set return type
                relation.ReturnType = valueType

            End If

        End If

        'Error
        If func Is Nothing And relation Is Nothing Then
            addNodeSyntaxError("VBCR03", "A ""return"" has nothing to do outside of a function or a relation.", node.value)
        End If

        'Compile
        content.Add("")
        content.Add("Return " & compileNode(node.value, content))
        content.Add("")

        'Return
        Return ""

    End Function

End Class
