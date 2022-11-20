Public Class Renderer

    '{CODES}'

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