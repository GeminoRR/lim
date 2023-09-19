'==============================
'========== CONTINUE ==========
'==============================
'
' Continue a loop
'
Class ContinueNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As ContinueNode = Me.MemberwiseClone()
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "continue"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Search parent loop
        Dim parent As StatementNode = Me
        Dim FindALoop As Boolean = False
        While parent.ParentNode IsNot Nothing
            parent = parent.ParentNode
            If TypeOf parent Is WhileNode Or TypeOf parent Is ForNode Or TypeOf parent Is ForeachNode Then
                FindALoop = True
                Exit While
            End If
        End While

        'Error
        If Not FindALoop Then
            ThrowNodeSyntaxException("CN01", """continue"" can only be used in a loop.", Me)
        End If

        'Compile
        Content.Add("continue;")

    End Sub

End Class
