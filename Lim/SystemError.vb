Module SystemError

    '=================================
    '========== BASIC ERROR ==========
    '=================================
    Public Sub addBasicError(ByVal name As String, ByVal message As String)

        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine(name.ToUpper() & ": " & message)
        Console.ResetColor()
        endApp()

    End Sub

    '===================================
    '========== BASIC WARNING ==========
    '===================================
    Public Sub addBasicWarning(ByVal name As String, ByVal message As String)

        Console.ForegroundColor = ConsoleColor.Yellow
        Console.WriteLine(name.ToUpper() & ": " & message)
        Console.ResetColor()
        endApp()

    End Sub

    '===================================
    '========== COMPLEX ERROR ==========
    '===================================
    Public Sub addComplexError(ByVal code As String, ByVal title As String, ByVal message As String, ByVal file As LimFile, ByVal positionStart As Integer, ByVal positionEnd As Integer, Optional ByVal trymessage As String = "")

        'Title
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine(title.ToUpper() & ": " & message)

        'Exeption
        Dim line As String = ""
        If (Not positionStart = -1) And (Not positionEnd = -1) Then

            line = getLinePosition(file.content, positionStart).ToString()
            Dim endLine As String = getLinePosition(file.content, positionEnd).ToString()
            Dim brutline As String = getLineFromPosition(file.content, line)

            Console.ResetColor()
            Console.WriteLine(line & "| " & brutline.Replace(vbTab, " "))

            If endLine = line Then
                Dim lineStartPosition = getLineStartPosition(file.content, positionStart)
                Console.WriteLine(StrDup(line.Length + 2, " ") & StrDup(positionStart - lineStartPosition, " ") & StrDup(positionEnd + 1 - positionStart, "^"))
            ElseIf endLine = line + 1 Then
                Console.WriteLine(endLine & "| " & getLineFromPosition(file.content, endLine).Replace(vbTab, " "))
            Else
                Console.WriteLine(StrDup(line.Length, " ") & "| ...")
                Console.WriteLine(endLine & "| " & getLineFromPosition(file.content, endLine).Replace(vbTab, " "))
            End If

        End If

        'Try
        If Not trymessage = "" Then
            Console.ForegroundColor = ConsoleColor.DarkGreen
            Console.WriteLine("Try :")
            Console.ResetColor()
            Console.WriteLine(vbTab & trymessage.Replace(Environment.NewLine, Environment.NewLine & vbTab))
        End If

        'Informations
        Console.ForegroundColor = ConsoleColor.Red
        If line = "" Then
            Console.WriteLine("<" & file.name & "> Code " & code)
        Else
            Console.WriteLine("<" & file.name & "> line " & line & " Code " & code)
        End If

        'Reset color
        Console.ResetColor()

        'End app
        endApp()

    End Sub

    '==================================
    '========== SYNTAX ERROR ==========
    '==================================
    Public Sub addSyntaxError(ByVal code As String, ByVal message As String, ByVal file As LimFile, ByVal positionStart As Integer, ByVal positionEnd As Integer, Optional ByVal trymessage As String = "")

        addComplexError(code, "SYNTAX ERROR", message, file, positionStart, positionEnd, trymessage)

    End Sub

    '=========================================
    '========== NODE INTERNAL ERROR ==========
    '=========================================
    Public Sub addNodeInternalError(ByVal code As String, ByVal message As String, ByVal node As Node, Optional ByVal trymessage As String = "")

        'Execute addSyntaxError
        addComplexError(code, "INTERNAL ERROR", message, getNodeParentFile(node), node.positionStart, node.positionEnd, trymessage)

    End Sub

    '=======================================
    '========== NODE SYNTAX ERROR ==========
    '=======================================
    Public Sub addNodeSyntaxError(ByVal code As String, ByVal message As String, ByVal node As Node, Optional ByVal trymessage As String = "")

        'Execute addSyntaxError
        addSyntaxError(code, message, getNodeParentFile(node), node.positionStart, node.positionEnd, trymessage)

    End Sub

    '==================================
    '========== NAMING ERROR ==========
    '==================================
    Public Sub addNamingError(ByVal code As String, ByVal message As String, ByVal file As LimFile, ByVal positionStart As Integer, ByVal positionEnd As Integer, Optional ByVal trymessage As String = "")

        addComplexError(code, "NAMING ERROR", message, file, positionStart, positionEnd, trymessage)

    End Sub

    '=======================================
    '========== NODE NAMING ERROR ==========
    '=======================================
    Public Sub addNodeNamingError(ByVal code As String, ByVal message As String, ByVal node As Node, Optional ByVal trymessage As String = "")

        'Execute addSyntaxError
        addNamingError(code, message, getNodeParentFile(node), node.positionStart, node.positionEnd, trymessage)

    End Sub

    '================================
    '========== TYPE ERROR ==========
    '================================
    Public Sub addTypeError(ByVal code As String, ByVal message As String, ByVal file As LimFile, ByVal positionStart As Integer, ByVal positionEnd As Integer, Optional ByVal trymessage As String = "")

        addComplexError(code, "TYPE ERROR", message, file, positionStart, positionEnd, trymessage)

    End Sub

    '=====================================
    '========== NODE TYPE ERROR ==========
    '=====================================
    Public Sub addNodeTypeError(ByVal code As String, ByVal message As String, ByVal node As Node, Optional ByVal trymessage As String = "")

        'Execute addSyntaxError
        addTypeError(code, message, getNodeParentFile(node), node.positionStart, node.positionEnd, trymessage)

    End Sub

    '=======================================
    '========== GET LINE POSITION ==========
    '=======================================
    Public Function getLinePosition(ByVal text As String, ByVal position As Integer) As Integer

        'Check
        If Not (position >= 0 And position < text.Length) Then
            Return -1
        End If

        'Count
        Dim line As Integer = 1
        For i As Integer = 0 To position
            If text(i) = Environment.NewLine Or text(i) = vbCr Then
                line += 1
            End If
        Next

        'Return
        Return line

    End Function

    '=============================================
    '========== GET LINE START POSITION ==========
    '=============================================
    Private Function getLineStartPosition(ByVal text As String, ByVal position As Integer) As Integer

        'Check
        If Not (position >= 0 And position < text.Length) Then
            Return -1
        End If

        'Count
        Dim line As Integer = 0
        For i As Integer = 0 To position
            If text(i) = Environment.NewLine Then
                line = i
            ElseIf text(i) = vbLf Then
                line = i + 1
            End If
        Next

        'Return
        Return line

    End Function

    '============================================
    '========== GET LINE FROM POSITION ==========
    '============================================
    Private Function getLineFromPosition(ByVal text As String, ByVal position As Integer) As String

        'Check
        position -= 1
        If Not (position >= 0 And position < text.Length) Then
            Return ""
        End If

        'Get lines
        Dim lines As String() = text.Split(Environment.NewLine)

        'Return
        Return lines(position)

    End Function


End Module
