'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Class FunctionNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReturnTypeNode As TypeNode
    Public FunctionName As String
    Public FunctionArguments As New List(Of FunctionArgumentNode)
    Public Export As Boolean
    Public Codes As New List(Of Node)
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
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal FunctionName As String, ByVal Export As Boolean)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.FunctionName = FunctionName
        Me.Export = Export

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim Export_STR As String = ""
        If Export Then
            Export_STR = "export "
        End If

        Dim Arguments_STR As String = ""
        If FunctionArguments.Count > 0 Then
            For Each arg As FunctionArgumentNode In FunctionArguments
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

        Return Export_STR & "func " & FunctionName & Arguments_STR & ReturnType_STR & Content_STR

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(ByVal content As List(Of String)) As String

        'Return
        Return ""

    End Function

End Class

'============================================
'========== FUNCTION ARGUMENT NODE ==========
'============================================
'
' Represents the argument of a function
'
Class FunctionArgumentNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ArgumentTypeNode As TypeNode
    Public ArgumentName As String
    Public ArgumentDefaultValue As ValueNode
    Public ReadOnly Property ArgumentType As Type
        Get
            If _ArgumentType Is Nothing Then

                If ArgumentTypeNode IsNot Nothing And ArgumentDefaultValue Is Nothing Then
                    'name:foo
                    _ArgumentType = ArgumentTypeNode.AssociateType

                ElseIf ArgumentTypeNode Is Nothing And ArgumentDefaultValue IsNot Nothing Then
                    'name = bar
                    If Not ArgumentDefaultValue.IsConstant Then
                        ThrowNodeSyntaxException("FNFAN01", "The default value of an argument must be a constant value.", ArgumentDefaultValue)
                    End If
                    _ArgumentType = ArgumentDefaultValue.ReturnType

                ElseIf ArgumentTypeNode IsNot Nothing And ArgumentDefaultValue IsNot Nothing Then
                    'name:foo = bar
                    If Not ArgumentDefaultValue.IsConstant Then
                        ThrowNodeSyntaxException("FNFAN02", "The default value of an argument must be a constant value.", ArgumentDefaultValue)
                    End If
                    If Not ArgumentTypeNode.AssociateType = ArgumentDefaultValue.ReturnType Then
                        ThrowNodeSyntaxException("FNFAN03", "Argument type (" & ArgumentTypeNode.AssociateType.ToString() & ") does not match default value type (<" & ArgumentDefaultValue.ReturnType.ToString() & ">).", ArgumentDefaultValue)
                    End If
                    _ArgumentType = ArgumentTypeNode.AssociateType

                End If

            End If
            Return _ArgumentType
        End Get
    End Property
    Private _ArgumentType As Type

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal ArgumentName As String, ByVal ArgumentTypeNode As TypeNode, ByVal ArgumentDefaultValue As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.ArgumentName = ArgumentName
        Me.ArgumentTypeNode = ArgumentTypeNode
        If Me.ArgumentTypeNode IsNot Nothing Then
            Me.ArgumentTypeNode.ParentNode = Me
        End If
        Me.ArgumentDefaultValue = ArgumentDefaultValue
        If Me.ArgumentDefaultValue IsNot Nothing Then
            Me.ArgumentDefaultValue.ParentNode = Me
        End If

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Dim Type_STR As String = ""
        If ArgumentTypeNode IsNot Nothing Then
            Type_STR = ":" & ArgumentTypeNode.ToString()
        End If
        Dim Value_STR As String = ""
        If ArgumentDefaultValue IsNot Nothing Then
            Value_STR = " = (" & ArgumentDefaultValue.ToString() & ")"
        End If
        Return ArgumentName & Type_STR & Value_STR
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(ByVal content As List(Of String)) As String

        'Return
        Return ""

    End Function

End Class