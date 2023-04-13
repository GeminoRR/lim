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
    Private flags As New List(Of String)
    Public debugLogs As Boolean

    '==============================
    '========== HAS FLAG ==========
    '==============================
    Public Function HasFlag(ByVal ShortName As String, ByVal LongName As String) As Boolean

        Return flags.Contains("-" & ShortName) Or flags.Contains("--" & LongName)

    End Function

    '======================================
    '========== MAIN ENTRY POINT ==========
    '======================================
    Sub Main(args As String())

        'App culture
        Threading.Thread.CurrentThread.CurrentUICulture = New System.Globalization.CultureInfo("en")

        'Start application
        MainApplication(args.ToList())

        'End
        EndApplication()

    End Sub

    Public Sub EndApplication()

#If DEBUG Then
        'To see the result when debugging
        Console.ReadKey()
#End If

    End Sub

    '=================================
    '========== ENTRY POINT ==========
    '=================================
    Sub MainApplication(ByVal args As List(Of String))

        'Flag | File
        For Each arg As String In args

            If arg.StartsWith("-") Then

                'Add flag
                flags.Add(arg)

            Else

                'Fix ""
                If arg.StartsWith("""") Then
                    arg = arg.Substring(1)
                End If
                If arg.EndsWith("""") Then
                    arg = arg.Substring(0, arg.Count - 1)
                End If

                'Add file
                files.Add(arg)

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

        'Compile
        If files.Count = 1 Then

            'Compile & Run
            compile(files(0))

        Else

            'Compile


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
    Public Sub ShowHelp()

        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("Usage :")
        Console.ResetColor()
        Console.WriteLine(vbTab & "lim <source> [flags]")
        Console.WriteLine(vbTab & vbTab & "Compiles and executes directly on the console the code of the file <source>.")
        Console.WriteLine("")
        Console.WriteLine(vbTab & "lim <source> <output> [flags]")
        Console.WriteLine(vbTab & vbTab & "Compiles the <sources> file to the <output> executable.")
        Console.WriteLine(vbTab & vbTab & "Warning, if a file already exists at the location <output> it will be overwritten.")

        Console.WriteLine("")
        Console.WriteLine("")
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("Flags :")
        Console.ResetColor()
        Console.WriteLine(vbTab & "-v" & vbTab & "--version")
        Console.WriteLine(vbTab & vbTab & vbTab & "Open the version menu")
        Console.WriteLine(vbTab & "-h" & vbTab & "--help")
        Console.WriteLine(vbTab & vbTab & vbTab & "Open the help menu")
        Console.WriteLine(vbTab & "-d" & vbTab & "--debug")
        Console.WriteLine(vbTab & vbTab & vbTab & "Displays the compilation steps.")



    End Sub

    '================================
    '========== SHOW GUIDE ==========
    '================================
    Public Sub ShowGuide()

        Console.ResetColor()
        Console.WriteLine("Invalid command, type ""lim -h"" for help.")

    End Sub


End Module
