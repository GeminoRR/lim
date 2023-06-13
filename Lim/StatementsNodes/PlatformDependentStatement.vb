'==================================================
'========== PLATFORM DEPENDENT STATEMENT ==========
'==================================================
'
' Represents a line that will be compiled only if the
' indicated platform is the same as the targeted compilation platform.
'
Class PlatformDependentStatement
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Private Line As StatementNode
    Private TargetedPlatform As Platform

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As PlatformDependentStatement = Me.MemberwiseClone()
        Cloned.Line = Cloned.Line.Clone(Cloned)
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal TargetedPlatform As Platform, ByVal Line As StatementNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, Line.PositionEndY, Line.PositionEndX)

        'Properties
        Me.TargetedPlatform = TargetedPlatform
        Me.Line = Line
        Me.Line.ParentNode = Me

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "($" & Me.TargetedPlatform.ToString() & ")" & Me.Line.ToString()
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Compile
        If Me.TargetedPlatform = CurentPlatform Then
            Me.Line.Compile(Content)
        End If
        If Me.TargetedPlatform = Platform.NoConsole And SystemConsole.HideConsole Then
            Me.Line.Compile(Content)
        ElseIf Me.TargetedPlatform = Platform.Console And Not Systemconsole.HideConsole Then
            Me.Line.Compile(Content)
        End If

    End Sub

End Class
