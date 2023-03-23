Imports System
Imports System.IO

Module Program

    '===============================
    '========== VARIABLES ==========
    '===============================
    Dim flags As New List(Of String)
    Dim arguments As New List(Of String)
    Dim debugLogs As Boolean = False
    Dim appFolder As String


    '================================
    '========== MAIN ENTRY ==========
    '================================
    Sub Main(args As String())

        'Get template folder
        appFolder = Directory.GetParent(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName).FullName.Replace("\", "/")
        templateFolder = appFolder & "/templates"
        If Not Directory.Exists(templateFolder) Then
            addBasicError("Folder not found", "The ""templates"" folder could not be found. Try reinstalling lim.")
        End If

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

            If flags.Contains("-h") Or flags.Contains("--help") Then
                showHelp()
            ElseIf flags.Contains("-v") Or flags.Contains("--version") Then
                showVersion()
            Else
                Console.WriteLine("Unable to execute the command. Use ""lim -h"" for help.")
            End If

            endApp()
        End If
        Dim inputFile As String = arguments(0)

        'Get output
        Dim outputFile As String = ""
        If arguments.Count > 1 Then
            outputFile = arguments(1)
            'If outputFile.StartsWith("""") Then
            '    outputFile = outputFile.Substring(1)
            'End If
            'If outputFile.EndsWith("""") Then
            '    outputFile = outputFile.Substring(0, outputFile.Length - 1)
            'End If
        End If

        'Debug
        If flags.Contains("-d") Or flags.Contains("--debug") Then
            debugLogs = True
        End If

        'Compile
        If outputFile = "" Then

            'Run

            'Compile to publish
            Dim executable As String = compileInPublish(inputFile)

            'Open
            'Process.Start("C:\Program Files\Sublime Text\subl.exe", """" & AppData & "/compiled/main.c""")

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

            'Wait until clode
            While Not runtime.HasExited
            End While

        Else

            'Fix output
            If Not outputFile.EndsWith(".exe") And Environment.OSVersion.Platform = PlatformID.Win32NT Then
                outputFile &= ".exe"
            End If

            'Compile to publish
            Dim executable As String = compileInPublish(inputFile)

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
    Function compileInPublish(ByVal inputFile As String) As String

        'Compile file
        Dim compiler As New C_Compiler()
        compiler.compileCode(inputFile, AppData & "/compiled", flags)

        'Set publish folder
        If Directory.Exists(AppData & "/publish") Then
            Directory.Delete(AppData & "/publish", True)
        End If
        Directory.CreateDirectory(AppData & "/publish")
        If Directory.Exists(AppData & "/icon") Then
            Directory.Delete(AppData & "/icon", True)
        End If

        'Icon
        Dim resPath As String = """" & templateFolder & "/My.res"""
        Dim iconPath As String = Nothing
        For Each flag As String In flags
            If flag.StartsWith("-i") Then
                iconPath = flag.Substring(2)
            ElseIf flag.StartsWith("--icon") Then
                iconPath = flag.Substring(6)
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
                addBasicError("File doesn't exist", iconPath & " doesn't exist")
            Else
                File.Copy(iconPath, AppData & "/icon/icon.ico", True)
            End If
            File.WriteAllText(AppData & "/icon/temp.rc", "id ICON ""icon.ico""")
            While Not File.Exists(AppData & "/icon/temp.rc")
            End While
            Dim windres As New Process()
            windres.StartInfo.FileName = templateFolder & "/mingw64/bin/windres.exe"
            windres.StartInfo.Arguments = "temp.rc -O coff -o my.res"
            windres.StartInfo.WorkingDirectory = AppData & "/icon"
            windres.Start()
            While Not windres.HasExited
            End While
            resPath = "../icon/my.res"
        End If

        'Executalbe zbi
        Dim gcc As New Process()
        gcc.StartInfo.FileName = templateFolder & "/mingw64/bin/gcc.exe"
        gcc.StartInfo.Arguments = "* -o ../publish/prog.exe " & resPath
        gcc.StartInfo.WorkingDirectory = AppData & "/compiled"
        gcc.Start()

        'Wait
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
        Return Nothing

    End Function

    '===========================
    '========== CLOSE ==========
    '===========================
    Public Sub endApp()
        If Not (flags.Contains("-k") Or flags.Contains("--keep")) Then
            If Directory.Exists(AppData & "/publish") Then
                Directory.Delete(AppData & "/publish", True)
            End If
            If Directory.Exists(AppData & "/icon") Then
                Directory.Delete(AppData & "/icon", True)
            End If
            If Directory.Exists(AppData & "/compiled") Then
                Directory.Delete(AppData & "/compiled", True)
            End If
        End If
        'Console.ReadKey()
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
        Console.WriteLine(vbTab & vbTab & "-i" & vbTab & "--icon" & vbTab & vbTab & "Select the executable icon.")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine(vbTab & vbTab & vbTab & vbTab & vbTab & "Exemple : -i""hello.ico""")
        Console.ResetColor()
        Console.WriteLine(vbTab & vbTab & "-k" & vbTab & "--keep" & vbTab & vbTab & "Does not delete temporary files.")
        Console.WriteLine(vbTab & vbTab & "-h" & vbTab & "--help" & vbTab & vbTab & "Show the help menu.")
        Console.WriteLine(vbTab & vbTab & "-v" & vbTab & "--version" & vbTab & vbTab & "Shows the current version of lim.")

        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine("*Lim is in beta. Many bugs are to be deplored*")
        Console.ResetColor()

    End Sub

    '==================================
    '========== SHOW VERSION ==========
    '==================================
    Public Sub showVersion()

        'Usage
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("Lim")
        Console.ResetColor()
        Dim fileinfo As FileVersionInfo = FileVersionInfo.GetVersionInfo(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName)
        Console.WriteLine("version: " & fileinfo.FileVersion)

    End Sub

End Module
