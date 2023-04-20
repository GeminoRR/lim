'===================================
'========== SYSTEM ERRORS ==========
'===================================
'
' This module takes care of interpreting and displaying errors.
'
Module LimExceptions

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
        For i As Integer = PositionStartY To PositionEndY

            Dim OriginalColor As ConsoleColor = Console.BackgroundColor
            Console.ResetColor()
            Console.Write((i + 1).ToString() & vbTab & "|")
            Console.BackgroundColor = OriginalColor

            For y As Integer = 0 To file.lines(i).Count - 1

                If i = PositionStartY And y = PositionStartX Then
                    Console.BackgroundColor = ConsoleColor.DarkRed
                End If
                Console.Write(file.lines(i)(y))
                If i = PositionEndY And y = PositionEndX Then
                    Console.ResetColor()
                End If

            Next

        Next

        'Help message
        If Not HelpMessage = Nothing Then
            Console.ResetColor()
            Console.ForegroundColor = ConsoleColor.DarkGreen
            Console.WriteLine(vbTab & vbTab & HelpMessage)
        End If

        'Bottom
        Dim lineToString As String
        If PositionStartY = PositionEndY Then
            lineToString = "line " & (PositionStartY + 1).ToString()
        Else
            lineToString = "lines " & (PositionStartY + 1).ToString() & " to " & (PositionEndY + 1).ToString()
        End If
        Console.ResetColor()
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("""" & file.filename & """, " & lineToString & ", code " & code)
        Console.ResetColor()

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

    '========================================
    '========== LIM NODE EXCEPTION ==========
    '========================================
    Public Sub ThrowNodeException(ByVal code As String, ByVal title As String, ByVal message As String, ByVal node As Node, Optional ByVal HelpMessage As String = Nothing)
        ThrowCoordinatesLimException(code, title, message, node.ParentFile, node.PositionStartY, node.PositionStartX, node.PositionEndY, node.PositionEndX, HelpMessage)
    End Sub

    '===============================================
    '========== LIM NODE SYNTAX EXCEPTION ==========
    '===============================================
    Public Sub ThrowNodeSyntaxException(ByVal code As String, ByVal message As String, ByVal node As Node, Optional ByVal HelpMessage As String = Nothing)
        ThrowCoordinatesLimException(code, "syntax error", message, node.ParentFile, node.PositionStartY, node.PositionStartX, node.PositionEndY, node.PositionEndX, HelpMessage)
    End Sub

    '=============================================
    '========== LIM NODE TYPE EXCEPTION ==========
    '=============================================
    Public Sub ThrowNodeTypeException(ByVal code As String, ByVal message As String, ByVal node As Node, Optional ByVal HelpMessage As String = Nothing)
        ThrowCoordinatesLimException(code, "type error", message, node.ParentFile, node.PositionStartY, node.PositionStartX, node.PositionEndY, node.PositionEndX, HelpMessage)
    End Sub

End Module