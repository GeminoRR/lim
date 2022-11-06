Imports System
Imports System.IO

Module Program

    '================================
    '========== MAIN ENTRY ==========
    '================================
    Sub Main(args As String())

        'Run mode ?
        If args.Count = 1 Then

            'File exist ?
            args(0) = args(0).Replace("\", "/")
            If Not File.Exists(args(0)) Then

                'Get filename
                Dim filename As String = args(0)
                If filename.Contains("/") Then
                    filename = args(0).Substring(args(0).LastIndexOf("/"))
                End If

                'Check file
                If File.Exists(args(0) & ".lim") Then

                    'File but missing .lim
                    args(0) &= ".lim"
                    addBasicWarning("file not found", "Did you mean """ & filename & ".lim"" ?")

                Else

                    'No file
                    addBasicError("file not found", "The file """ & filename & """ does not exist.")

                End If
            End If

            'Lim file ?
            If Not args(0).EndsWith(".lim") Then

                'No file
                addBasicError("incorrect file", "The specified file is not a lim source file (does not end with .lim)")

            End If

            'Compile
            Dim compiler As New VB_Compiler(args(0), AppData & "/Compiled")

            'Run
            'Process.Start("cmd.exe", "/c cd """ & AppData & "/Compiled" & """ & dotnet run")

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
        Console.WriteLine("RUN : lim <input_file>")
        Console.WriteLine("COMPILE: lim <input_file> <output_file> [-arguments]")

        'Explains
        Console.ResetColor()
        Console.WriteLine("<input_file>" & vbTab & "Path of the .lim file to compile")
        Console.WriteLine("<output_file>" & vbTab & "Path of the future executable file. (This will be created by the compiler)")
        Console.WriteLine("[-arguments]" & vbTab & "Optional. Argument list.")
        Console.WriteLine(vbTab & "-vb" & vbTab & vbTab & "Compiles to a .vb file (VisualBasic)")
        Console.WriteLine(vbTab & "-c" & vbTab & vbTab & "Compiles to a .c file (C)")
        Console.WriteLine(vbTab & "-windows" & vbTab & "Compiles to a .exe file (Executable)")
        Console.WriteLine(vbTab & "-linux" & vbTab & vbTab & "Compiles to a linux executable")
        Console.WriteLine(vbTab & "-macos" & vbTab & vbTab & "Compiles to a MacOS executable")
        Console.WriteLine("If no arguments are entered, lim will compile to your operating system's executable.")

    End Sub

End Module
