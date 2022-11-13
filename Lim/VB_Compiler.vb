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

    Private stdInt As ClassNode
    Private strFloat As ClassNode
    Private stdStr As ClassNode
    Private stdBool As ClassNode
    Private stdFun As ClassNode

    '================================
    '========== MAIN ENTRY ==========
    '================================
    Public Sub New(ByVal inputFile As String, ByVal outputFolder As String)

        'Restore variables
        compiledVariables = ""
        compiledClasss = ""
        compiledFunctions = ""
        variableCount = 0
        helpVariableCount = 0
        functionCount = 0
        classCount = 0
        compileType = compileWay.console

        'Get template
        Dim templateFolder As String = System.Reflection.Assembly.GetExecutingAssembly().Location().Replace("\", "/")
        templateFolder = templateFolder.Substring(0, templateFolder.LastIndexOf("/")) & "/templates/vb"

        'Add dimensions
        compiledClasss &= Environment.NewLine & Environment.NewLine & Helper.getTemplate(templateFolder, "dimensions.vb")

        'Analyse first file
        entryFile = New LimFile(inputFile, Me)
        files.Add(entryFile)

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
        stdInt = getClass("int", entryFile)
        strFloat = getClass("float", entryFile)
        stdStr = getClass("str", entryFile)
        stdBool = getClass("bool", entryFile)
        stdFun = getClass("fun", entryFile)

        'Compile start
        compileFunction(entryPoint)

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
        addLog("Finish code compiling")

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
        If Not Directory.Exists(templateFolder & "/compileEnvironment/" & folderTemplatePath) Then
            addBasicError("File missing", "The """ & folderTemplatePath & """ model folder could not be found. Try reinstalling Lim.")
        End If

        'Copy template
        Microsoft.VisualBasic.FileIO.FileSystem.CopyDirectory(templateFolder & "/compileEnvironment/" & folderTemplatePath, outputFolder)

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
                'If graphicsDrawFunction Is Nothing Then
                '    addCustomSyntaxWarning("VBCW01", "No ""drawFrame"" function was found in the __init__ space. The window will therefore remain empty throughout the execution of the program. Proposal: Add the following function: ""func drawFrame(@screen:image)""", mainFile.name)
                'Else
                '    Program = Program.Replace("'{DRAWFRAME}'", graphicsDrawFunction.compiledName)
                'End If

        End Select

        'Write file
        Try
            File.WriteAllText(outputFolder & "/Program.vb", Program)
        Catch ex As Exception
            addBasicError("Cannot write file", ex.Message)
        End Try

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
        compileStruct(TypeClass)
        Return New safeType(TypeClass, typenode.Dimensions)

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
                compileStruct(currentClass)
                Return currentClass
            End If
        Next

        'Search in others files
        For Each file As LimFile In currentFile.FilesImports
            For Each currentClass As ClassNode In file.classs
                If currentClass.Name = name And currentClass.export Then
                    compileStruct(currentClass)
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

    '====================================
    '========== COMPILE STRUCT ==========
    '====================================
    Private Sub compileStruct(ByRef currentClass As ClassNode)

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

        'Add source directly
        If currentClass.addSourceDirectly.Count > 0 Then
            content.Add("")
            content.Add("'Add sources directly")
            For Each source As AddSourceNode In currentClass.addSourceDirectly
                content.Add(source.value)
            Next
        End If

        'Get variables
        If currentClass.declareVariables.Count > 0 Then

            content.Add("")
            content.Add("'Variables")
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
                    content.Add(String.Format("Public {0} As {1} = {2}", var.compiledName, compileSafeType(var.type), compileNode(def.value, content)))

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

        'Get functions
        If currentClass.methods.Count > 0 Then
            content.Add("")
            content.Add("'Methods")
            For Each def As FunctionNode In currentClass.methods
                content.Add("")
                content.Add(compileFunction(def).Replace(Environment.NewLine, Environment.NewLine & vbTab))
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

        'Get name
        If fun.compiledName = "" Then
            If getNodeParentFile(fun).LimLib Then
                fun.compiledName = fun.Name
            Else
                fun.compiledName = getFunctionName()
            End If
        End If

        'Clone
        If Not parentClass Is Nothing And fun.Name = "clone" Then
            fun.compiling = False
            Return "'clone" & Environment.NewLine & "Public Function clone() As " & compileSafeType(fun.ReturnType) & Environment.NewLine & vbTab & "Return DirectCast(Me.MemberwiseClone(), " & compileSafeType(fun.ReturnType) & ")" & Environment.NewLine & "End Function"
        End If

        'Some variables
        Dim content As New List(Of String)

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
            compiled_arguments &= ", " & var.compiledName & " As " & compileSafeType(var.type)
            fun.variables.Add(var)
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
            finalString &= Environment.NewLine & Environment.NewLine & vbTab & "'Return empty" & Environment.NewLine & vbTab & "Return Nothing" & Environment.NewLine & Environment.NewLine & "End Function"
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
                    Return New safeType(strFloat)

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

        End If

        'binOpNode
        If TypeOf node Is binOpNode Then

            'Casted node
            Dim castedNode As binOpNode = DirectCast(node, binOpNode)

            'Get type
            Dim left As safeType = getNodeType(castedNode.leftNode)
            Dim right As safeType = getNodeType(castedNode.rightNode)

            'Handle
            If left.Dimensions.Count > 0 Then
                addNodeTypeError("VBGNT02", "The subtraction operation cannot be performed on a list", castedNode.leftNode)
            End If
            If right.Dimensions.Count > 0 Then
                addNodeTypeError("VBGNT03", "The subtraction operation cannot be performed on a list", castedNode.rightNode)
            End If

            'Type
            Select Case castedNode.op.type
                Case tokenType.OP_MINUS
                    If left.TargetClass.compiledName = right.TargetClass.compiledName Then
                        Return New safeType(stdInt)
                    End If
                    Return New safeType(strFloat)

                Case tokenType.OP_PLUS
                    If left.TargetClass.compiledName = "str" And right.TargetClass.compiledName = "str" Then
                        Return New safeType(stdStr)
                    ElseIf left.TargetClass.compiledName = "int" And right.TargetClass.compiledName = "int" Then
                        Return New safeType(stdInt)
                    End If
                    Return New safeType(strFloat)

                Case tokenType.OP_MULTIPLICATION
                    If left.TargetClass.compiledName = "int" And right.TargetClass.compiledName = "int" Then
                        Return New safeType(stdInt)
                    ElseIf left.TargetClass.compiledName = "int" And right.TargetClass.compiledName = "str" Then
                        Return New safeType(stdStr)
                    ElseIf left.TargetClass.compiledName = "str" And right.TargetClass.compiledName = "int" Then
                        Return New safeType(stdStr)
                    End If
                    Return New safeType(strFloat)

                Case tokenType.OP_MODULO
                    Return New safeType(stdInt)
                    'TODO: Better modulo type

                Case tokenType.OP_DIVISION
                    Return New safeType(strFloat)
                    'TODO: Better modulo type

                Case Else
                    'Problem go brrr
                    Throw New NotImplementedException()

            End Select

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

        'ChildNode
        If TypeOf node Is childNode Then

            'Castednode
            Dim castedNode As childNode = DirectCast(node, childNode)

            'Get struct
            'Dim struct As ClassNode = getStructOf(castedNode.parentStruct)
            'TODO: fix this badboi

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

        ElseIf TypeOf node Is AddSourceNode Then

            'Check if file is limlib
            If getNodeParentFile(node).LimLib Then

                'Compile
                content.Add(DirectCast(node, AddSourceNode).value)
                Return ""

            Else

                'Error
                addNodeSyntaxError("VBCCN02", "It is not possible to integrate source code outside of a ""limlib"" file.", node)

            End If

        End If

        'Return
        addNodeTypeError("VBCCN01", "Unable to resolve node type", node)
        Return Nothing

    End Function

    '======================================
    '========== COMPILE VARIABLE ==========
    '======================================
    Public Function compileVariable(ByVal node As VariableNode, ByVal content As List(Of String)) As String

        'Get variable
        Dim var As Variable = getVariable(node.VariableName, node)

        'Return
        Return var.compiledName

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
            addNodeTypeError("VBCUO02", "The """ & node.op.ToString() & """ operator cannot be applied to a <" & valueType.ToString() & ">", node)
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
        addNodeTypeError("VBCUO01", "The """ & node.op.ToString() & """ operator cannot be applied to a <" & valueType.ToString() & ">", node)
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
            addNodeTypeError("VBCBS01", "Chosen item must be at least map or list to select index/key", node.Target)
        End If
        If Not {ValueType.list, ValueType.map}.Contains(targetType.Dimensions(targetType.Dimensions.Count - 1)) Then
            addNodeTypeError("VBCBS02", "Chosen item must be at least map or list to select index/key", node.Target)
        End If

        'Get type
        Dim indexType As safeType = getNodeType(node.index)

        'Compile
        Select Case targetType.Dimensions(targetType.Dimensions.Count - 1)
            Case ValueType.list
                If Not (indexType.TargetClass.compiledName = "int" And indexType.Dimensions.Count = 0) Then
                    addNodeTypeError("VBCBS03", "An index of a list must be an integer (<int>)", node.index)
                End If
                Return compileNode(node.Target, content) & "(" & compileNode(node.index, content) & ".value)"

            Case ValueType.map
                If Not (indexType.TargetClass.compiledName = "str" And indexType.Dimensions.Count = 0) Then
                    addNodeTypeError("VBCBS04", "An key name of a map must be an string (<str>)", node.index)
                End If
                Return compileNode(node.Target, content) & "(" & compileNode(node.index, content) & ".value)"

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

        'Return
        Select Case node.op.type

            Case tokenType.OP_PLUS
                'ADDITIONS

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCBO01", "Both elements cannot be a list to allow a ""+"" operation.", node)
                End If

                'Number operation
                If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "int" Then
                    Return String.Format("New int({0}.value + {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New float({0}.value + {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

                'String operation
                If leftType.TargetClass.compiledName = "str" And rightType.TargetClass.compiledName = "str" Then
                    Return String.Format("New str({0}.value & {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_MINUS
                'SUBSTRACTION

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCBO02", "Both elements cannot be a list to allow a ""-"" operation.", node)
                End If

                'Number operation
                If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "int" Then
                    Return String.Format("New int({0}.value - {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New float({0}.value - {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

            Case tokenType.OP_MULTIPLICATION
                'MULTIPLICATION

                'Check error
                If leftType.Dimensions.Count > 0 Or rightType.Dimensions.Count > 0 Then
                    addNodeTypeError("VBCBO03", "Both elements cannot be a list to allow a ""*"" operation.", node)
                End If

                'Number operation
                If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "int" Then
                    Return String.Format("New int({0}.value * {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If
                If (leftType.TargetClass.compiledName = "int" Or leftType.TargetClass.compiledName = "float") And (rightType.TargetClass.compiledName = "int" Or rightType.TargetClass.compiledName = "float") Then
                    Return String.Format("New float({0}.value * {1}.value)", compileNode(node.leftNode, content), compileNode(node.rightNode, content))
                End If

                'String & number
                If leftType.TargetClass.compiledName = "int" And rightType.TargetClass.compiledName = "str" Then
                    Dim helpVar As String = getHelpVariableName()
                    Dim iVar As String = getHelpVariableName()
                    content.Add(String.Format("Dim {0} As String = """"", helpVar))
                    content.Add(String.Format("For {0} As Integer = 0 To {1}.value - 1", iVar, compileNode(node.leftNode, content)))
                    content.Add(String.Format("{0}{1} &= {2}.value", vbTab, helpVar, compileNode(node.rightNode, content)))
                    content.Add("Next")
                    Return String.Format("New str({0})", helpVar)
                End If

                'Number & String
                If leftType.TargetClass.compiledName = "str" And rightType.TargetClass.compiledName = "int" Then
                    Dim helpVar As String = getHelpVariableName()
                    Dim iVar As String = getHelpVariableName()
                    content.Add(String.Format("Dim {0} As String = """"", helpVar))
                    content.Add(String.Format("For {0} As Integer = 0 To {1}.value - 1", iVar, compileNode(node.rightNode, content)))
                    content.Add(String.Format("{0}{1} &= {2}.value", vbTab, helpVar, compileNode(node.leftNode, content)))
                    content.Add("Next")
                    Return String.Format("New str({0})", helpVar)
                End If

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

    '===========================================
    '========== COMPILE FUNCTION CALL ==========
    '===========================================
    Private Function compileFunctionCall(ByVal node As FunctionCallNode, ByVal content As List(Of String)) As String

        'Get function
        Dim fun As FunctionNode = getFunction(node.FunctionName, node)

        'Handle argument error
        If node.Arguments.Count < fun.Arguments.Count Then
            addNodeTypeError("VBCFC01", (fun.Arguments.Count - node.Arguments.Count).ToString() & " arguments are missing", node)
        End If
        If node.Arguments.Count > fun.Arguments.Count Then
            addNodeTypeError("VBCFC02", (node.Arguments.Count - fun.Arguments.Count).ToString() & " arguments are useless (too many arguments)", node)
        End If

        'Argument
        Dim arguments As String = ""
        For i As Integer = 0 To fun.Arguments.Count - 1

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
            addNodeTypeError("VBCSV01", "Cannot change a value of type <" & variableType.ToString() & "> to <" & valueType.ToString() & ">", castedNode)
        End If

        'Compile
        content.Add(String.Format("{0} = {1}.Clone()", compileNode(castedNode.Target, content), compileNode(castedNode.NewValue, content)))

        'Return
        Return ""

    End Function

    '==============================================
    '========== COMPILE DECLARE VARIABLE ==========
    '==============================================
    Private Function compileDeclareVariable(ByVal node As DeclareVariableNode, ByVal content As List(Of String)) As String

        'Declare variable
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
        Return ""

    End Function

    '====================================
    '========== COMPILE RETURN ==========
    '====================================
    Private Function compileReturn(ByVal node As ReturnNode, ByVal content As List(Of String)) As String

        'Variables
        Dim func As FunctionNode = getNodeParentFunction(node)
        Dim valueType As safeType = getNodeType(node.value)

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

        'Compile
        content.Add("Return " & compileNode(node.value, content))

        'Return
        Return ""

    End Function

End Class
