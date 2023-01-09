'================================
'========= SET VARIABLE =========
'================================
Public Class SetVariableNode
    Inherits Node

    'Variable
    Public Target As Node
    Public NewValue As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Target As Node, ByVal NewValue As Node)
        MyBase.New(positionStart, positionEnd)
        Me.Target = Target
        Me.Target.parentNode = Me
        Me.NewValue = NewValue
        Me.NewValue.parentNode = Me
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return Target.ToString() & " = " & NewValue.ToString()
    End Function

    'Clone
    Public Function clone() As SetVariableNode

        'Create new me
        Dim new_me As SetVariableNode = Me.MemberwiseClone()

        new_me.NewValue = new_me.NewValue.clone()
        new_me.NewValue.parentNode = Me

        'Return
        Return new_me

    End Function

End Class

'====================================
'========= DECLARE VARIABLE =========
'====================================
Public Class DeclareVariableNode
    Inherits Node

    'Variables
    Public variableName As String
    Public variableUnsafeType As typeNode
    Public value As Node
    Public declarationType As VariableDeclarationType

    Public export As Boolean = False

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal declarationType As VariableDeclarationType, ByVal variableName As String, ByVal value As Node, ByVal variableUnsafeType As typeNode)
        MyBase.New(positionStart, positionEnd)
        Me.variableUnsafeType = variableUnsafeType
        If Not variableUnsafeType Is Nothing Then
            Me.variableUnsafeType.parentNode = Me
        End If
        Me.variableName = variableName
        Me.declarationType = declarationType
        Me.value = value
        If Not value Is Nothing Then
            Me.value.parentNode = Me
        End If
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim valueSTR As String = ""
        If Not value Is Nothing Then
            valueSTR = " = " & value.ToString()
        End If
        Dim unsafeTypeSTR As String = ""
        If Not variableUnsafeType Is Nothing Then
            unsafeTypeSTR = ":" & variableUnsafeType.ToString()
        End If
        Dim declareSTR As String = "UnknownDeclaration"
        Select Case declarationType
            Case VariableDeclarationType._let_
                declareSTR = "LET"
            Case VariableDeclarationType._var_
                declareSTR = "VAR"
        End Select
        Return declareSTR & " " & variableName & unsafeTypeSTR & valueSTR
    End Function

End Class
Public Enum VariableDeclarationType
    _let_
    _var_
End Enum

'===================================
'========= WHILE STATEMENT =========
'===================================
Public Class whileStatementNode
    Inherits containerNode

    'Variable
    Public condition As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal condition As Node)
        MyBase.New(positionStart, positionEnd)
        Me.condition = condition
        Me.condition.parentNode = Me
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "(While " & condition.ToString() & " Do " & Me.codes.Count.ToString() & " things)"
    End Function

End Class

'=================================
'========= FOR STATEMENT =========
'=================================
Public Class forStatementNode
    Inherits containerNode

    'Variable
    Public looperTarget As Node
    Public variableName As String
    Public variableDeclareType As VariableDeclarationType

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal looperTarget As Node, ByVal variableName As String, ByVal variableDeclareType As VariableDeclarationType)
        MyBase.New(positionStart, positionEnd)
        Me.looperTarget = looperTarget
        Me.looperTarget.parentNode = Me
        Me.variableName = variableName
        Me.variableDeclareType = variableDeclareType
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim declarationType As String = ""
        Select Case variableDeclareType
            Case VariableDeclarationType._let_
                declarationType = "let "
            Case VariableDeclarationType._var_
                declarationType = "var"
        End Select

        Return "(For " & declarationType & " " & variableName & " in (" & looperTarget.ToString() & ") do " & Me.codes.Count.ToString() & " things)"
    End Function

End Class

'================================
'========= IF STATEMENT =========
'================================
Public Class ifStatementNode
    Inherits containerNode

    'Variable
    Public condition As Node
    Public if_statements As List(Of Node)
    Public elseif_statements As List(Of Tuple(Of Node, List(Of Node)))
    Public else_statement As List(Of Node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, condition As Node, ByVal if_statement As List(Of Node), ByVal elseif_statements As List(Of Tuple(Of Node, List(Of Node))), ByVal else_statement As List(Of Node))
        MyBase.New(positionStart, positionEnd)

        Me.condition = condition
        Me.condition.parentNode = Me

        Me.if_statements = if_statement
        For Each statement As Node In Me.if_statements
            statement.parentNode = Me
        Next

        Me.elseif_statements = elseif_statements
        For Each statement As Tuple(Of Node, List(Of Node)) In Me.elseif_statements
            statement.Item1.parentNode = Me
            For Each statement2 As Node In statement.Item2
                statement2.parentNode = Me
            Next
        Next

        Me.else_statement = else_statement
        For Each statement As Node In Me.else_statement
            statement.parentNode = Me
        Next

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "(If " & Me.condition.ToString() & " Do ...)"
    End Function

End Class