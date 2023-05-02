'===========================================
'========== DECLARE VARIABLE NODE ==========
'===========================================
'
' Represents the declaration of a variable
'
' Can be used in :
'   - Main program
'   - Class
'   - Scope
'
Class DeclareVariableNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public VariableName As String
    Public CustomVariableName As Boolean
    Public ExplicitType As TypeNode
    Public ExplicitValue As ValueNode
    Public Export As Boolean = False
    Private _VariableType As Type = Nothing
    Public ReadOnly Property VariableType As Type
        Get
            If _VariableType Is Nothing Then

                If ExplicitType IsNot Nothing And ExplicitValue Is Nothing Then
                    'let foobar:bar
                    _VariableType = ExplicitType.AssociateType

                ElseIf ExplicitType Is Nothing And ExplicitValue IsNot Nothing Then
                    'let foobar = foo
                    _VariableType = ExplicitValue.ReturnType

                Else
                    'let foobar:bar = foo
                    If Not ExplicitType.AssociateType = ExplicitValue.ReturnType Then
                        ThrowNodeTypeException("DVNVT01", "The explicit type (" & ExplicitType.AssociateType.ToString() & ") is not the same as the type of the value (" & ExplicitValue.ReturnType.ToString() & ").", Me)
                    End If
                    _VariableType = ExplicitType.AssociateType

                End If

            End If
            Return _VariableType
        End Get
    End Property



    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal VariableName As String, ByVal CustomName As Boolean, ByVal ExplicitType As TypeNode, ByVal ExplicitValue As ValueNode, ByVal Export As Boolean)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.VariableName = VariableName
        Me.CustomVariableName = CustomName
        Me.ExplicitType = ExplicitType
        If Me.ExplicitType IsNot Nothing Then
            Me.ExplicitType.ParentNode = Me
        End If
        Me.ExplicitValue = ExplicitValue
        If Me.ExplicitValue IsNot Nothing Then
            Me.ExplicitValue.ParentNode = Me
        End If
        Me.Export = Export

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim CustomName_STR As String = ""
        If CustomVariableName Then
            CustomName_STR = "$"
        End If

        Dim ExplicitType_STR As String = ""
        If ExplicitType IsNot Nothing Then
            ExplicitType_STR = ":" & ExplicitType.ToString()
        End If

        Dim ExplicitValue_STR As String = ""
        If ExplicitValue IsNot Nothing Then
            ExplicitValue_STR = " = (" & ExplicitValue.ToString() & ")"
        End If

        Return "(let " & CustomName_STR & VariableName & ExplicitType_STR & ExplicitValue_STR & ")"

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Sub Compile(Content As List(Of String))

        'Create variable
        Dim var As Variable
        If CustomVariableName Then
            var = New Variable(Me.VariableName, Me.VariableType, False, Me.VariableName, Me.Export)
        Else
            var = New Variable(Me.VariableName, Me.VariableType, False, Nothing, Me.Export)
        End If
        Me.ParentScope.Variables.Add(var)

        'Compile value
        Dim DefaultValue As String = ""
        If ExplicitValue Is Nothing Then
            DefaultValue = "NULL"
        Else
            DefaultValue = ExplicitValue.Compile(Content)
        End If

        'Compile
        Content.Add("")
        Content.Add(Me.VariableType.CompiledName & " * " & var.CompiledName & " = " & DefaultValue & ";")

    End Sub

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Function CompileFor(ByVal Create As List(Of String), ByVal Init As List(Of String), Optional InitWithSelf As Boolean = False) As Variable

        'Create variable
        Dim var As Variable
        If CustomVariableName Then
            var = New Variable(Me.VariableName, Me.VariableType, False, Me.VariableName, Me.Export)
        Else
            var = New Variable(Me.VariableName, Me.VariableType, False, Nothing, Me.Export)
        End If
        Me.ParentScope.Variables.Add(var)

        'Compile value
        Dim DefaultValue As String = ""
        If ExplicitValue Is Nothing Then
            DefaultValue = "NULL"
        Else
            DefaultValue = ExplicitValue.Compile(Compiled_Inits)
        End If

        'Compile
        Create.Add(Me.VariableType.CompiledName & " * " & var.CompiledName & ";")
        If InitWithSelf Then
            Init.Add("self->" & var.CompiledName & " = " & DefaultValue & ";")
        Else
            Init.Add(var.CompiledName & " = " & DefaultValue & ";")
        End If

        'Return
        Return var

    End Function

End Class
