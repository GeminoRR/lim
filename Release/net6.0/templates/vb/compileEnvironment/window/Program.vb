Public Class Renderer

    '{CODES}'


    '///////////////////////////
    '/////// KEY PRESSED ///////
    '///////////////////////////
    Public Function checkKeyPressed(ByVal key As String) As Boolean
        key = key.ToUpper()
        Select Case key
            Case "CTRL"
                key = "ControlKey"
            Case "ALT"
                key = "Menu"
            Case "ESCAPE"
                key = "Escape"
            Case "ENTER"
                key = "Return"
            Case "BACK"
                key = "Back"
            Case "TAB"
                key = "Tab"
            Case "SHIFT"
                key = "ShiftKey"
            Case "SPACE"
                key = "Space"
            Case "LEFT"
                key = "Left"
            Case "RIGHT"
                key = "Right"
            Case "UP"
                key = "Up"
            Case "DOWN"
                key = "Down"
        End Select
        For Each currentKey As Keys In keysPressed
            If currentKey.ToString() = key Then
                Return True
            End If
        Next
        Return False
    End Function

    Dim keysPressed As New List(Of Keys)

    Private Shadows Sub KeyDown(sender As Object, e As KeyEventArgs) Handles MyBase.KeyDown
        If Not keysPressed.Contains(e.KeyCode) Then
            keysPressed.Add(e.KeyCode)
        End If
    End Sub

    Private Shadows Sub KeyUp(sender As Object, e As KeyEventArgs) Handles MyBase.KeyUp
        If keysPressed.Contains(e.KeyCode) Then
            keysPressed.Remove(e.KeyCode)
        End If
    End Sub

    '/////////////////////////////
    '/////// MOUSE HANDLER ///////
    '/////////////////////////////
    Public Function getMouseX() As Integer
        Return Cursor.Position.X - Me.Location.X - 9
    End Function

    Public Function getMouseY() As Integer
        Return Cursor.Position.Y - Me.Location.Y - 31
    End Function

    '///////////////////////////
    '/////// WINDOW SIZE ///////
    '///////////////////////////
    Public Function getWindowWidth() As Boolean
        Return Me.Width
    End Function
    Public Function getWindowHeight() As Boolean
        Return Me.Height
    End Function

    '//////////////////////////
    '/////// PUSH ERROR ///////
    '//////////////////////////
    Public Sub pushError(ByVal message As String)
        MsgBox(message, MsgBoxStyle.Critical, "LIM RUNTIME ERROR")
        End
    End Sub

    '///////////////////////////
    '/////// ENTRY POINT ///////
    '///////////////////////////
    Private Sub Renderer_Load() Handles MyBase.Load
        screen = New image(New int(Me.Width), New int(Me.Height))
        frameRefreshTimer.Interval = Math.Round(1000 / 60)
        Try
            '{ENTRY_POINT}'
        Catch ex As Exception
            pushError(ex.Message)
        End Try
        frameRefreshTimer.Start()
    End Sub

    '//////////////////////////
    '/////// DRAW FRAME ///////
    '//////////////////////////
    Dim screen As image
    Private Sub frameRefreshTimer_Tick() Handles frameRefreshTimer.Tick

        Try
            '{DRAWFRAME}'(screen)
        Catch ex As Exception
            pushError(ex.Message)
        End Try
        display.Image = screen.img        
        display.Refresh()

    End Sub

End Class