'===============================
'========== TYPE NODE ==========
'===============================
'
' Represents a type node
'
Class TypeNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ClassName As String
    Public PassedArguments As New List(Of TypeNode)
    Public ReadOnly Property AssociateType As Type
        Get
            If _AssociateType Is Nothing Then
                'TODO: Compile the type
            End If
            Return _AssociateType
        End Get
    End Property
    Private _AssociateType As Type = Nothing

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal ClassName As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.ClassName = ClassName

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Arguments_STR As String = ""
        If PassedArguments.Count > 0 Then
            For Each arg As TypeNode In PassedArguments
                If arg Is Nothing Then
                    Arguments_STR &= ", Nothing"
                Else
                    Arguments_STR &= ", (" & arg.ToString() & ")"
                End If
            Next
            Arguments_STR = "<" & Arguments_STR.Substring(2) & ">"
        End If
        Return ClassName & Arguments_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String
        Return Me.AssociateType.CompiledName
    End Function

End Class