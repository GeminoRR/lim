'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Class RelationNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReturnTypeNode As TypeNode
    Public RelationOperator As Token
    Public RelationArguments As New List(Of FunctionArgumentNode)
    Public Codes As New List(Of Node)
    Public ReadOnly CompiledName As String
    Public ReadOnly Property ReturnType As Type
        Get
            If _ReturnType Is Nothing Then
                If ReturnTypeNode Is Nothing Then
                    _ReturnType = Nothing
                Else
                    _ReturnType = ReturnTypeNode.AssociateType
                End If
            End If
            Return _ReturnType
        End Get
    End Property
    Private _ReturnType As Type

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal RelationOperator As Token)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.RelationOperator = RelationOperator
        Me.CompiledName = GetRelationCompiledName()

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim Arguments_STR As String = ""
        If RelationArguments.Count > 0 Then
            For Each arg As FunctionArgumentNode In RelationArguments
                Arguments_STR &= ", " & arg.ToString()
            Next
            Arguments_STR = "(" & Arguments_STR.Substring(2) & ")"
        End If

        Dim ReturnType_STR As String = ""
        If ReturnTypeNode IsNot Nothing Then
            ReturnType_STR = ":" & ReturnTypeNode.ToString()
        End If

        Dim Content_STR As String = ""
        If Codes.Count > 0 Then
            For Each content As Node In Codes
                Content_STR &= Environment.NewLine & content.ToString()
            Next
            Content_STR = Environment.NewLine & "(" & Content_STR & Environment.NewLine & ")"
        End If

        Return "relation " & RelationOperator.ToString() & Arguments_STR & ReturnType_STR & Content_STR

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(ByVal Content As List(Of String))
        Throw New NotImplementedException()
    End Sub

    '===========================
    '========== CLONE ==========
    '===========================
    Public Function Clone() As RelationNode
        Return Me.MemberwiseClone()
    End Function

End Class