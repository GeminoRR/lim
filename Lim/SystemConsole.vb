Imports System.IO
'====================================
'========== SYSTEM CONSOLE ==========
'====================================
'
' Manage user interaction from the console.
'

Module SystemConsole

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private files As New List(Of String)
    Public flags As New List(Of String)
    Public ShowDebug As Boolean
    Public CurentPlatform As Platform
    Public HideConsole As Boolean = False

    '==============================
    '========== HAS FLAG ==========
    '==============================
    Public Function HasFlag(ByVal ShortName As String, ByVal LongName As String) As Boolean

        Return flags.Contains("-" & ShortName.ToLower()) Or flags.Contains("--" & LongName.ToLower())

    End Function

    '======================================
    '========== MAIN ENTRY POINT ==========
    '======================================
    Sub Main(args As String())

        'Get executable directory
        If Not Directory.Exists(Compiler.executableDirectory) Then
            ThrowSimpleLimException("SCM01", "Folder missing", "Unable to find the executable folder. Try reinstalling lim.")
        End If

        'App culture
        Threading.Thread.CurrentThread.CurrentUICulture = New System.Globalization.CultureInfo("en")
        CurentPlatform = Platform.Win

        'Start application
        MainApplication(args.ToList())

        'End
        EndApplication()

    End Sub

    Public Sub EndApplication()

        'Keep
        If Not HasFlag("k", "keep") Then
            Try
                For Each filepath As String In Directory.GetFiles(AppData & "/src")
                    File.Delete(filepath)
                Next
                For Each folder As String In Directory.GetDirectories(AppData & "/src")
                    If Not folder.Replace("\", "/") = AppData & "/src/gc" Then
                        Directory.Delete(folder, True)
                    End If
                Next
                For Each filepath As String In Directory.GetFiles(AppData & "/bin")
                    File.Delete(filepath)
                Next
                For Each folder As String In Directory.GetDirectories(AppData & "/bin")
                    Directory.Delete(folder, True)
                Next
            Catch ex As Exception
            End Try
        End If

        'To see the result when debugging
#If DEBUG Then
        Console.ReadKey()
#End If

        'Close
        End

    End Sub

    '=================================
    '========== ENTRY POINT ==========
    '=================================
    Sub MainApplication(ByVal args As List(Of String))

        'Flag | File
        For Each arg As String In args

            If arg.StartsWith("-") Then

                'Add flag
                flags.Add(arg.ToLower())

            Else

                'Fix ""
                If arg.StartsWith("""") Then
                    arg = arg.Substring(1)
                End If
                If arg.EndsWith("""") Then
                    arg = arg.Substring(0, arg.Count - 1)
                End If

                'Add file
                files.Add(arg.ToLower())

            End If

        Next

        'No compilation
        If files.Count = 0 Then

            'Version
            If HasFlag("v", "version") Then
                ShowVersion()
                Return
            End If

            'Help
            If HasFlag("h", "help") Then
                ShowHelp()
                Return
            End If

            'Else
            ShowGuide()
            Return

        End If

        'Debug
        ShowDebug = HasFlag("d", "debug")
        HideConsole = HasFlag("hc", "hideconsole")

        'Compile platform
        If HasFlag("l", "linux") Then
            CurentPlatform = Platform.Linux
        ElseIf HasFlag("w", "windows") Then
            CurentPlatform = Platform.Win
        End If

        'Compile
        If files.Count = 1 Then

            'Get current dir
            Dim CurrentDir As String = Path.GetFullPath(files(0)).Replace("\", "/")
            If CurrentDir.Contains("/") Then
                CurrentDir = CurrentDir.Substring(0, CurrentDir.LastIndexOf("/"))
            End If

            'Compile & Run file
            Dim P As New Process()
#If DEBUG Then
            P.StartInfo.FileName = Compiler.compile(files(0))
#Else
            Try
                P.StartInfo.FileName = Compiler.compile(files(0))
            Catch ex As Exception
                If ShowDebug Then
                    Console.ForegroundColor = ConsoleColor.DarkGray
                    Console.WriteLine(ex.Message)
                End If
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Compilation failed!")
                Console.ResetColor()
                EndApplication()
            End Try
#End If
            P.StartInfo.WorkingDirectory = CurrentDir
            P.Start()
            While Not P.HasExited
                Threading.Thread.Sleep(1)
            End While
            Threading.Thread.Sleep(1)
            EndApplication()

        Else

            'Fix
            If Not files(1).EndsWith(".exe") Then
                files(1) = files(1) & ".exe"
            End If

            'Compile
#If DEBUG Then
            File.Move(Compiler.compile(files(0)), files(1), True)
            For Each CompiledFile As String In Directory.GetFiles(AppData & "/bin")
                CompiledFile = CompiledFile.Replace("\", "/")
                File.Move(CompiledFile, Directory.GetParent(files(1)).FullName & "/" & CompiledFile.Substring(CompiledFile.LastIndexOf("/") + 1), True)
            Next
#Else
Try
                File.Move(Compiler.compile(files(0)), files(1), True)
                For Each CompiledFile As String In Directory.GetFiles(AppData & "/bin")
                    CompiledFile = CompiledFile.Replace("\", "/")
                    File.Move(CompiledFile, Directory.GetParent(files(1)).FullName & "/" & CompiledFile.Substring(CompiledFile.LastIndexOf("/") + 1), True)
                Next
            Catch ex As Exception
                If ShowDebug Then
                    Console.ForegroundColor = ConsoleColor.DarkGray
                    Console.WriteLine(ex.Message)
                End If
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Compilation failed!")
                Console.ResetColor()
                EndApplication()
            End Try
#End If


            'Finished
            Console.ForegroundColor = ConsoleColor.DarkGreen
            Console.WriteLine("Compilation successful!")
            Console.ResetColor()

        End If

    End Sub

    '==================================
    '========== SHOW VERSION ==========
    '==================================
    Public Sub ShowVersion()

        Console.ResetColor()
        Console.WriteLine("Lim version: 0.0.2.0")

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
        Console.WriteLine(vbTab & vbTab & vbTab & vbTab & vbTab & "Exemple : --icon=hello.ico")
        Console.ResetColor()
        Console.WriteLine(vbTab & vbTab & "-k" & vbTab & "--keep" & vbTab & vbTab & "Does not delete temporary files.")
        Console.WriteLine(vbTab & vbTab & "-h" & vbTab & "--help" & vbTab & vbTab & "Show the help menu.")
        Console.WriteLine(vbTab & vbTab & "-v" & vbTab & "--version" & vbTab & "Shows the current version of lim.")
        Console.WriteLine(vbTab & vbTab & "-l" & vbTab & "--linux" & vbTab & vbTab & "Compiles an executable that can only be used on GNU/Linux.")
        Console.WriteLine(vbTab & vbTab & "-w" & vbTab & "--windows" & vbTab & "Compiles an executable that can only be used on Windows.")
        Console.WriteLine(vbTab & vbTab & "-hc" & vbTab & "--hideconsole" & vbTab & "The final executable will not open a console.")

        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine("*Lim is in beta. Many bugs are to be deplored*")
        Console.ResetColor()

    End Sub

    '================================
    '========== SHOW GUIDE ==========
    '================================
    Public Sub ShowGuide()

        Console.ResetColor()
        Console.WriteLine("Invalid command, type ""lim -h"" for help.")

    End Sub


End Module
Enum Platform
    Win
    Linux

    NoConsole
    Console
End Enum