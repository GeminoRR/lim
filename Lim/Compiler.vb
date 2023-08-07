Imports System.ComponentModel
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

    Public AllFiles As New List(Of String)

    Public DefinedTypes As New List(Of Type)
    Public TypeIDCounter As Integer = 0
    Public ClassIDCounter As Integer = 0

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
    Public Function compile(ByVal inputFile As String) As String

        'Reset build environment
        Try

            'bin directory
            If Not Directory.Exists(AppData & "/bin") Then
                Directory.CreateDirectory(AppData & "/bin")
            End If

            'src directory
            If Not Directory.Exists(AppData & "/src") Then
                Directory.CreateDirectory(AppData & "/src")
            End If

        Catch ex As Exception
            ThrowSimpleLimException("CC01", "Unable to reset build environment", ex.Message)
        End Try

        'Add main file
        MainFile = New SourceFile(inputFile)
        AllImportedFiles.Add(MainFile)
        Status("Files parsing: done")

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
            If MainFunction.FunctionArguments.Count > 1 Then
                ThrowNodeSyntaxException("CC03", "The entry point can only have zero or one argument.", MainFunction.FunctionArguments(1))
            End If
            If Not MainFunction.FunctionArguments(0).ArgumentTypeNode.ClassName = "list" Then
                ThrowNodeSyntaxException("CC13", "The ""main"" function has an array of type ""list<str>"" as its only argument.", MainFunction.FunctionArguments(0).ArgumentTypeNode)
            End If
            If Not MainFunction.FunctionArguments(0).ArgumentTypeNode.PassedArguments.Count = 1 Then
                ThrowNodeSyntaxException("CC14", "The ""main"" function has an array of type ""list<str>"" as its only argument.", MainFunction.FunctionArguments(0).ArgumentTypeNode)
            End If
            If Not MainFunction.FunctionArguments(0).ArgumentTypeNode.PassedArguments(0).ClassName = "str" Then
                ThrowNodeSyntaxException("CC15", "The ""main"" function has an array of type ""list<str>"" as its only argument.", MainFunction.FunctionArguments(0).ArgumentTypeNode.PassedArguments(0))
            End If
        End If
        If MainFunction.ReturnTypeNode IsNot Nothing Then
            ThrowNodeSyntaxException("CC04", "The entry point cannot return a value.", MainFunction.ReturnTypeNode)
        End If

        'Handle extends
        For Each file As SourceFile In AllImportedFiles
            For Each extend As ExtendNode In file.Extends

                'Search
                Dim TargetedClass As ClassNode = Nothing
                For Each ClassN As ClassNode In file.Classes

                    If ClassN.ClassName = extend.ExtendTarget Then
                        TargetedClass = ClassN
                        Exit For
                    End If
                Next
                If TargetedClass Is Nothing Then
                    For Each importedF As SourceFile In file.ImportedFiles
                        If TargetedClass IsNot Nothing Then
                            Exit For
                        End If
                        For Each ClassN As ClassNode In importedF.Classes
                            If ClassN.ClassName = extend.ExtendTarget Then
                                TargetedClass = ClassN
                                Exit For
                            End If
                        Next
                    Next
                End If

                'Result
                If TargetedClass Is Nothing Then
                    ThrowNodeNamingException("CC12", "Unable to find class """ & extend.ExtendTarget & """. Check that it has been correctly defined / imported.", extend)
                End If
                If Not TargetedClass.ParentFile.filepath = extend.ParentFile.filepath Then
                    TargetedClass.ParentFile.ImportFile(extend.ParentFile.filepath)
                End If
                If TypeOf extend.Content Is FunctionNode Then
                    Dim Clone As FunctionNode = extend.Content.Clone()
                    Clone.ParentNode = TargetedClass
                    TargetedClass.Methods.Add(Clone)
                ElseIf TypeOf extend.Content Is RelationNode Then
                    Dim Clone As RelationNode = extend.Content.Clone()
                    Clone.ParentNode = TargetedClass
                    TargetedClass.Relations.Add(Clone)
                Else
                    Throw New NotImplementedException()
                End If

            Next
        Next
        Status("Extension handling: done")

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
        Status("Logic Compilation: done")

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

        'No console
        If SystemConsole.HideConsole Then
            If Not Compiled_Imports.Contains("#include <SDL.h>") Then
                Compiled_Imports.Add("#include <SDL.h>")
            End If
        End If

        'Use SDL ?
        Dim UseSDL As Boolean = False
        For Each importedLib As String In Compiled_Imports
            If importedLib = "#include <SDL.h>" Then
                UseSDL = True
                Exit For
            End If
        Next

        'Find new & add method of list<str>
        Dim list_str As Type = Nothing
        Dim list_str_new As FunctionNode = Nothing
        Dim list_str_add As FunctionNode = Nothing
        If MainFunction.FunctionArguments.Count > 0 Then
            list_str = GetTypeFromClassAndArgs(Nothing, STDClass_list, {STD_str}.ToList())
            For Each method As FunctionNode In list_str.Methods
                If method.FunctionName = "new" Then
                    list_str_new = method
                    method.Compile(Nothing)
                ElseIf method.FunctionName = "add" Then
                    list_str_add = method
                    method.Compile(Nothing)
                End If
            Next
        End If

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

            'Variables
            SW.WriteLine("")
            SW.WriteLine("///////////////////////")
            SW.WriteLine("////// VARIABLES //////")
            SW.WriteLine("///////////////////////")
            SW.WriteLine("typedef struct global_variables{")
            For Each line As String In Compiled_Variables
                SW.WriteLine(vbTab & line)
            Next
            SW.WriteLine("} global_variables;")

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
            SW.WriteLine("static tgc_t gc;")
            For Each line As String In Compiled_AddSourceDirectly
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
            Dim SecondMain As String = GetFunctionCompiledName()
            SW.WriteLine("void * " & SecondMain & "(int argc, char **argv){")
            If UseSDL Then
                SW.WriteLine(vbTab & "")
                SW.WriteLine(vbTab & "//Initialize SDL")
                SW.WriteLine(vbTab & "if (SDL_Init(SDL_INIT_VIDEO) != 0){")
                SW.WriteLine(vbTab & "  ThrowRuntimeError(""SDL:  %s"", SDL_GetError());")
                SW.WriteLine(vbTab & "}")
            End If
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Initializing global variable values")
            SW.WriteLine(vbTab & "global_variables * GV = tgc_alloc(&gc, sizeof(global_variables));")
            For Each line As String In Compiled_Inits
                SW.WriteLine(vbTab & line)
            Next
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Calls the ""main"" function")
            If MainFunction.FunctionArguments.Count > 0 Then
                Dim list_str_name As String = GetVariableCompiledName()
                SW.WriteLine(vbTab & list_str.CompiledName & " * " & list_str_name & " = " & list_str_new.CompiledName & " (GV);")
                SW.WriteLine(vbTab & "for (int i = 0; i < argc; i++){")
                SW.WriteLine(vbTab & vbTab & list_str_add.CompiledName & "(GV, " & list_str_name & ", new_str(argv[i]));")
                SW.WriteLine(vbTab & "}")
                SW.WriteLine(vbTab & MainFunction.CompiledName & "(GV, NULL, " & list_str_name & ");")
            Else
                SW.WriteLine(vbTab & MainFunction.CompiledName & "(GV, NULL);")
            End If
            SW.WriteLine(vbTab & "")
            SW.WriteLine("}")

            SW.WriteLine("")

            SW.WriteLine("int main(int argc, char **argv){")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Initializing random")
            SW.WriteLine(vbTab & "srand(time(NULL));")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Starting garbage collector")
            SW.WriteLine(vbTab & "tgc_start(&gc, &argc);")
            SW.WriteLine(vbTab & "")

            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Calls the second main function")
            SW.WriteLine(vbTab & "void * (*volatile entrypoint)(int, char**) = " & SecondMain & ";")
            SW.WriteLine(vbTab & "entrypoint(argc, argv);")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Stop the garbage collector")
            SW.WriteLine(vbTab & "tgc_stop(&gc);")
            SW.WriteLine(vbTab & "")
            SW.WriteLine(vbTab & "//Exit program")
            SW.WriteLine(vbTab & "return 0;")
            SW.WriteLine(vbTab & "")
            SW.WriteLine("}")

        End Using
        Status("Assembly of main.c: done")

        '=== OLD CODE STOLEN FROM LAST RELEASE ===

        'Icon directory
        If Directory.Exists(AppData & "/icon") Then
            Directory.Delete(AppData & "/icon", True)
        End If

        'Icon
        Dim resPath As String = """" & executableDirectory & "/my.res"""
        Dim iconPath As String = Nothing
        For Each flag As String In flags
            If flag.StartsWith("-i=") Then
                iconPath = flag.Substring(3)
            ElseIf flag.StartsWith("--icon=") Then
                iconPath = flag.Substring(7)
            End If
        Next
        If iconPath IsNot Nothing Then
            Directory.CreateDirectory(AppData & "/icon")
            If iconPath.StartsWith("""") Then
                iconPath = iconPath.Substring(1)
            End If
            If iconPath.EndsWith("""") Then
                iconPath = iconPath.Substring(0, iconPath.Length - 1)
            End If
            If Not File.Exists(iconPath) Then
                ThrowSimpleLimException("CC12", "File doesn't exist", iconPath & " doesn't exist")
            Else
                File.Copy(iconPath, AppData & "/icon/icon.ico", True)
            End If
            File.WriteAllText(AppData & "/icon/temp.rc", "id ICON ""icon.ico""")
            While Not File.Exists(AppData & "/icon/temp.rc")
            End While
            Dim windres As New Process()
            windres.StartInfo.FileName = executableDirectory & "/mingw64/bin/windres.exe"
            windres.StartInfo.Arguments = "temp.rc -O coff -o my.res"
            windres.StartInfo.WorkingDirectory = AppData & "/icon"
            windres.Start()
            While Not windres.HasExited
            End While
            resPath = "../icon/my.res"
        End If

        'Get all files
        Dim FilesRef As String = ""
        For Each val As String In AllFiles
            FilesRef &= " """ & val & """"
        Next

        'Executalbe
        Dim gcc As New Process()
        gcc.StartInfo.FileName = executableDirectory & "/mingw64/bin/gcc.exe"
        Dim CompileCommand As String = AppData & "/src/main.c" & FilesRef & " -O2 -o ../bin/prog.exe " & resPath
        If UseSDL Then
            CompileCommand &= " -I""" & executableDirectory & "/SDL/include"" -L""" & executableDirectory & "/SDL/lib"" -lmingw32 -lSDL2main -lSDL2"
        End If
        If SystemConsole.HideConsole Then
            CompileCommand &= " -mwindows"
        End If
        gcc.StartInfo.Arguments = CompileCommand
        gcc.StartInfo.WorkingDirectory = AppData & "/src"
        gcc.StartInfo.UseShellExecute = False
        gcc.StartInfo.RedirectStandardError = True
        gcc.StartInfo.CreateNoWindow = True
        gcc.Start()

        If ShowDebug Then

            Console.CursorVisible = False
            Dim CursorPos As ValueTuple(Of Integer, Integer) = Nothing
            Dim EndCursorPos As ValueTuple(Of Integer, Integer) = Nothing
            Console.Write("[LOGS] Compiling C to Bin [")
            CursorPos = Console.GetCursorPosition()
            Console.Write("-]" & Environment.NewLine)
            EndCursorPos = Console.GetCursorPosition()
            Dim LoadingState = 0

            While Not gcc.HasExited
                Console.SetCursorPosition(CursorPos.Item1, CursorPos.Item2)
                LoadingState += 1
                Select Case LoadingState
                    Case 1
                        Console.Write("-")
                    Case 2
                        Console.Write("\")
                    Case 3
                        Console.Write("|")
                    Case 4
                        Console.Write("/")
                    Case Else
                        LoadingState = 0
                End Select
                Threading.Thread.Sleep(10)
            End While

            Console.SetCursorPosition(EndCursorPos.Item1, EndCursorPos.Item2)
            Console.CursorVisible = True

        Else

            gcc.WaitForExit()

        End If
        Dim output As String = gcc.StandardError.ReadToEnd()

        'Create run.bat
        If HasFlag("k", "keep") Then
            File.WriteAllText(AppData & "/run.bat", "cd """ & gcc.StartInfo.WorkingDirectory & """" & Environment.NewLine & gcc.StartInfo.FileName & " " & gcc.StartInfo.Arguments & Environment.NewLine & "cd ..")
        End If

        'Move SDL dll
        If UseSDL Then
#If DEBUG Then
            File.Copy(executableDirectory & "/SDL/bin/SDL2.dll", AppData & "/bin/SDL2.dll", True)
#Else
            Try
                File.Copy(executableDirectory & "/SDL/bin/SDL2.dll", AppData & "/bin/SDL2.dll", True)
            Catch ex As Exception
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Compilation failed!")
                Console.ResetColor()
                EndApplication()
            End Try
#End If

        End If

        'File Compiled
        If gcc.ExitCode = 0 Then

            Status("C file compilation: done")
            For Each file As String In Directory.GetFiles(AppData & "/bin")
                If file.EndsWith(".exe") Then
                    Return file
                End If
            Next

        End If

        'Error
        Status("C file compilation: fail")
        Console.ForegroundColor = ConsoleColor.DarkRed
        Console.WriteLine("Compilation failed!")
        Console.ResetColor()
        If ShowDebug Then
            Console.WriteLine("GCC ERROR:")
            Console.WriteLine(output)
        End If
        EndApplication()
        Return Nothing

    End Function

    '=========================
    '========== LOG ==========
    '=========================
    Friend Sub Log(ByVal message As String)
        If ShowDebug Then
            Console.WriteLine("[LOGS] " & message)
        End If
    End Sub

    '===========================
    '========== SATUS ==========
    '===========================
    Friend Sub Status(ByVal message As String)
        If ShowDebug Then
            Console.WriteLine("[STATUS] " & message)
        End If
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