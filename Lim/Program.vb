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

        'Compile
        If outputFile = "" Then

            'Run
            Dim compiler As New C_Compiler()
            compiler.compileCode(inputFile, flags)

            'Open
            Process.Start("C:\Program Files\Sublime Text\subl.exe", """" & AppData & "/compiled/main.c""")

            'Run
            'Throw New NotImplementedException()
            'Dim run As New Process()
            ''run.StartInfo.FileName = "dotnet"
            ''run.StartInfo.Arguments = "run --project """ & AppData & "/compiled/VB.vbproj"""
            'run.StartInfo.WorkingDirectory = Directory.GetCurrentDirectory()
            'run.Start()

            ''Wait until programs end
            'While Not run.HasExited
            '    Threading.Thread.Sleep(200)
            'End While

            ''End app
            'endApp()

        Else

            'Fix output
            If Not outputFile.EndsWith(".exe") Then
                outputFile &= ".exe"
            End If

            'Get compiling target
            If flags.Contains("-c") Or flags.Contains("--c") Then

                'C
                addBasicError("feature unavailable", "Compilation on the C language is not yet available")

            ElseIf flags.Contains("-m") Or flags.Contains("--macos") Then

                'MacOs
                addBasicError("feature unavailable", "Compilation to MacOs is not yet available")

            ElseIf flags.Contains("-l") Or flags.Contains("--linux") Then

                'Linux
                addBasicError("feature unavailable", "Compilation to Linux is not yet available")

            Else

                'Windows
                Dim compiler As New C_Compiler()
                compiler.compileCode(inputFile, flags)

                'Executalbe zbi
                Throw New NotImplementedException()

            End If

            'End app
            endApp()

        End If

    End Sub

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

        'Example
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("RUN : Lim <input_file>")
        Console.WriteLine("COMPILE: lim <input_file> <output_file> [-arguments]")
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.WriteLine("*Lim is in beta. Many bugs are to be deplored*")

        'Explains
        Console.ResetColor()
        Console.WriteLine("<input_file>" & vbTab & "Path of the .lim file to compile")
        Console.WriteLine("<output_file>" & vbTab & "Path of the future executable file. (This will be created by the compiler)")
        Console.WriteLine("[-arguments]" & vbTab & "Optional. Argument list.")
        Console.WriteLine(vbTab & "-c" & vbTab & "--c" & vbTab & vbTab & "Compiles to a .c file (C)")
        Console.WriteLine(vbTab & "-w" & vbTab & "--windows" & vbTab & "Compiles to a .exe file (Executable)")
        Console.WriteLine(vbTab & "-l" & vbTab & "--linux" & vbTab & vbTab & "Compiles to a linux executable")
        Console.WriteLine(vbTab & "-m" & vbTab & "--macos" & vbTab & vbTab & "Compiles to a MacOS executable")
        Console.WriteLine(vbTab & "-l" & vbTab & "--logs" & vbTab & vbTab & "Show logs")
        Console.WriteLine("If no arguments are entered, lim will compile to your operating system's executable.")

    End Sub

End Module
