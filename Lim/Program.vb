Imports System
Imports System.IO

Module Program

    '================================
    '========== MAIN ENTRY ==========
    '================================
    Sub Main(args As String())

        'Get template folder
        templateFolder = System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName
        templateFolder = Directory.GetParent(templateFolder).FullName.Replace("\", "/") & "/templates"
        If Not Directory.Exists(templateFolder) Then
            addBasicError("Folder not found", "The ""templates"" folder could not be found. Try reinstalling lim.")
        End If

        'Variables
        Dim flags As New List(Of String)
        Dim arguments As New List(Of String)

        'Parse command
        For Each arg As String In args

            If arg.StartsWith("-") Then
                flags.Add(arg)
            Else
                arguments.Add(arg)
            End If

        Next

        'Get input
        If Not arguments.Count > 0 Then
            showHelp()
            endApp()
        End If
        Dim inputFile As String = arguments(0)

        'Get output
        Dim outputFile As String = ""
        If arguments.Count > 1 Then
            outputFile = arguments(1)
        End If

        'Debug
        Dim debugLogs As Boolean = False
        If flags.Contains("-d") Or flags.Contains("--debug") Then
            debugLogs = True
        End If

        'Compile
        If outputFile = "" Then

            'Run

            'Compile to publish
            Dim executable As String = compileInPublish(inputFile, flags)

            'Open
            Process.Start("C:\Program Files\Sublime Text\subl.exe", """" & AppData & "/compiled/main.c""")

            'Error
            If executable = Nothing Then
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Compilation failed!")
                Console.ResetColor()
            End If

            'Start process
            Dim runtime As New Process()
            runtime.StartInfo.FileName = executable
            runtime.StartInfo.WorkingDirectory = Directory.GetCurrentDirectory()
            runtime.Start()


        Else

            'Fix output
            If Not outputFile.EndsWith(".exe") And Environment.OSVersion.Platform = PlatformID.Win32NT Then
                outputFile &= ".exe"
            End If

            'Compile to publish
            Dim executable As String = compileInPublish(inputFile, flags)

            'Error
            If executable = Nothing Then
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Compilation failed!")
                Console.ResetColor()
            End If

            'Move file
            File.Move(executable, outputFile, True)

            'Finished
            Console.ForegroundColor = ConsoleColor.DarkGreen
            Console.WriteLine("Compilation successful!")
            Console.ResetColor()

        End If

        'End app
        endApp()

    End Sub

    '========================================
    '========== COMPILE IN PUBLISH ==========
    '========================================
    Function compileInPublish(ByVal inputFile As String, ByVal flags As List(Of String)) As String

        'Compile file
        Dim compiler As New C_Compiler()
        compiler.compileCode(inputFile, AppData & "/compiled", flags)

        'Set publish folder
        If Directory.Exists(AppData & "/publish") Then
            Directory.Delete(AppData & "/publish", True)
        End If
        Directory.CreateDirectory(AppData & "/publish")

        'Executalbe zbi
        Dim gcc As New Process()
        gcc.StartInfo.FileName = templateFolder & "/mingw64/bin/gcc.exe"
        gcc.StartInfo.Arguments = "* -o ../publish/prog.exe """ & templateFolder & "/my.res"""
        gcc.StartInfo.WorkingDirectory = AppData & "/compiled"
        gcc.Start()

        'Wait
        Dim debugLogs As Boolean = flags.Contains("-d") Or flags.Contains("--debug")
        If debugLogs Then
            Console.Write("[")
        End If
        While Not gcc.HasExited
            If debugLogs Then
                Console.Write("~")
            End If
            Threading.Thread.Sleep(5)
        End While
        If debugLogs Then
            Console.Write("]" & Environment.NewLine)
        End If

        'File Compiled
        If gcc.ExitCode = 0 Then

            Dim publishFiles() As String = Directory.GetFiles(AppData & "/publish")
            If Not publishFiles.Count = 1 Then
                Return Nothing
            End If
            Return publishFiles(0)

        End If

        'Return nothing
        Return gcc.ExitCode

    End Function

    '===========================
    '========== CLOSE ==========
    '===========================
    Public Sub endApp()
        Console.ReadKey()
        End
    End Sub

    '===============================
    '========== SHOW HELP ==========
    '===============================
    Public Sub showHelp()

        'Usage
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("USAGE:")
        Console.ResetColor()
        Console.WriteLine(vbTab & "lim <input_file>")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine(vbTab & vbTab & "Run the application in the console directly.")
        Console.ResetColor()
        Console.WriteLine("")
        Console.WriteLine(vbTab & "lim <input_file> <output_file> [-arguments]")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine(vbTab & vbTab & "Compile to an executable.")

        'Arguments
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("")
        Console.WriteLine("ARGUMENTS:")
        Console.ResetColor()
        Console.WriteLine(vbTab & "<input_file>" & vbTab & "Path of the .lim file to compile")
        Console.WriteLine(vbTab & "<output_file>" & vbTab & "Path of the future executable file. (This will be created by the compiler)")
        Console.WriteLine(vbTab & "[-arguments]" & vbTab & "Optional. Argument list.")
        Console.WriteLine(vbTab & vbTab & "-d" & vbTab & "--debug" & vbTab & vbTab & "Show debug logs")

        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine("*Lim is in beta. Many bugs are to be deplored*")
        Console.ResetColor()

    End Sub

End Module
