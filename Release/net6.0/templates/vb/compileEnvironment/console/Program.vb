Imports System

Module Program'{CODES}'

	'///////////////////////////
	'/////// ENTRY POINT ///////
	'///////////////////////////
	Sub Main()
        Try
            '{ENTRY_POINT}'
        Catch ex As Exception
            pushError(ex.Message)
        End Try
	End Sub

    '//////////////////////////
    '/////// PUSH ERROR ///////
    '//////////////////////////
    Public Sub pushError(ByVal message As String)
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine("LIM RUNTIME ERROR")
        Console.ResetColor()
        Console.WriteLine(message)
        End
    End Sub

End Module