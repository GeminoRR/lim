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

    '///////////////////////////
    '/////// ENTRY POINT ///////
    '///////////////////////////
    Private Sub Renderer_Load() Handles MyBase.Load
        screen = New image(New int(Me.Width), New int(Me.Height))
        frameRefreshTimer.Interval = Math.Round(1000 / 60)
        '{ENTRY_POINT}'
        frameRefreshTimer.Start()
    End Sub

    '//////////////////////////
    '/////// DRAW FRAME ///////
    '//////////////////////////
    Dim screen As image
    Private Sub frameRefreshTimer_Tick() Handles frameRefreshTimer.Tick

        '{DRAWFRAME}'(screen)
        display.Image = screen.img        
        display.Refresh()

    End Sub

End Class