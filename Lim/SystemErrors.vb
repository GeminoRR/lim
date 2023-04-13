'===================================
'========== SYSTEM ERRORS ==========
'===================================
'
' This module takes care of interpreting and displaying errors.
'
Public Module LimExceptions

    '==========================================
    '========== LIM SIMPLE EXCEPTION ==========
    '==========================================
    Public Sub ThrowSimpleLimException(ByVal code As String, ByVal name As String, ByVal message As String)

        'Exception
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine(name.ToUpper() & ": " & message)

        'Bottom
        Console.WriteLine("code " & code)

        'Finish application
        Console.ResetColor()
        EndApplication()

    End Sub

    '===============================================
    '========== LIM COORDINATES EXCEPTION ==========
    '===============================================
    Public Sub ThrowCoordinatesLimException(ByVal code As String, ByVal name As String, ByVal message As String, ByVal file As SourceFile, ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, Optional ByVal HelpMessage As String = Nothing)

        'Exception
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine(name.ToUpper() & ": " & message)

        'Get lines
        Console.ResetColor()
        For i As Integer = PositionStartY To PositionEndY

            Console.Write((i + 1).ToString() & vbTab & "|")

            For y As Integer = 0 To file.lines(i).Count - 1

                If i = PositionStartY And y = PositionStartX Then
                    Console.BackgroundColor = ConsoleColor.DarkRed
                End If
                Console.Write(file.lines(i)(y))
                If i = PositionEndY And y = PositionEndX Then
                    Console.ResetColor()
                End If

            Next
            Console.Write(Environment.NewLine)

        Next

        'Bottom
        Dim lineToString As String
        If PositionStartY = PositionEndY Then
            lineToString = "line " & (PositionStartY + 1).ToString()
        Else
            lineToString = "lines " & (PositionStartY + 1).ToString() & " to " & (PositionEndY + 1).ToString()
        End If
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("""" & file.filename & """, " & lineToString & ", code " & code)

        'Finish application
        Console.ResetColor()
        EndApplication()

    End Sub

    '======================================================
    '========== LIM COORDINATES SYNTAX EXCEPTION ==========
    '======================================================
    Public Sub ThrowCoordinatesSyntaxLimException(ByVal code As String, ByVal message As String, ByVal file As SourceFile, ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, Optional ByVal HelpMessage As String = Nothing)
        ThrowCoordinatesLimException(code, "syntax error", message, file, PositionStartY, PositionStartX, PositionEndY, PositionEndX, HelpMessage)
    End Sub

End Module