'==============================
'========== FUNCTION ==========
'==============================
Public Class FunctionNode
    Inherits containerNode

    'Variable
    Public Name As String
    Public Arguments As List(Of FunctionArgument)
    Public maxArguments As Integer
    Public minArguments As Integer

    Public ReturnType As Type = Nothing
    Public unsafeReturnType As typeNode = Nothing

    Public compiledName As String = ""
    Public compiled As Boolean
    Public compiling As Boolean

    Public export As Boolean = False
    Public AddSourceDirectly As AddSourceNode = Nothing

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String, ByVal Arguments As List(Of FunctionArgument), ByVal unsafeReturnType As typeNode)
        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.Arguments = Arguments
        Me.maxArguments = Me.Arguments.Count
        Me.minArguments = 0
        Dim lastArgWasOptional As Boolean = False
        For Each arg As FunctionArgument In Me.Arguments
            If arg.type IsNot Nothing Then
                arg.type.parentNode = Me
            End If
            If arg.value IsNot Nothing Then
                arg.value.parentNode = Me
                lastArgWasOptional = True
            ElseIf lastArgWasOptional Then
                addNodeSyntaxError("NFN01", "A non-optional argument cannot follow an optional argument", Me, "Put optional arguments at the end of the argument list.")
            Else
                Me.minArguments += 1
            End If
        Next
        Me.unsafeReturnType = unsafeReturnType
        If Me.unsafeReturnType IsNot Nothing Then
            Me.unsafeReturnType.parentNode = Me
        End If
        Me.compiled = False
        Me.compiling = False
    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Export
        Dim export As String = ""
        If Me.export Then
            export = "export "
        End If

        'Argument
        Dim argumentsSTR As String = ""
        If Arguments.Count > 0 Then
            For Each arg As FunctionArgument In Arguments
                argumentsSTR &= ", " & arg.ToString()
            Next
            argumentsSTR = argumentsSTR.Substring(2)
        End If

        'Return type
        Dim returnTypeSTR As String = ""
        If ReturnType IsNot Nothing Then
            returnTypeSTR = ":" & Me.ReturnType.ToString()
        End If

        'Return
        Return String.Format("({0}function {1}({2}){3})", export, Me.Name, argumentsSTR, returnTypeSTR)

    End Function

    'Clone
    Public Function clone() As FunctionNode
        Return Me.MemberwiseClone()
    End Function

End Class
Public Class FunctionArgument

    Public name As String
    Public type As typeNode
    Public declareType As VariableDeclarationType
    Public value As Node
    Public Sub New(ByVal name As String, ByVal type As typeNode, ByVal declareType As VariableDeclarationType, Optional value As Node = Nothing)
        Me.name = name
        Me.type = type
        Me.declareType = declareType
        Me.value = value
    End Sub

    Public Overrides Function ToString() As String

        If declareType = VariableDeclarationType._let_ Then

            'Ref
            Dim result As String = name
            If type IsNot Nothing Then
                result &= ":" & type.ToString()
            End If
            If value IsNot Nothing Then
                result &= " = " & value.ToString()
            End If
            Return result

        Else

            'Copy
            'Ref
            Dim result As String = "var " & name
            If type IsNot Nothing Then
                result &= ":" & type.ToString()
            End If
            If value IsNot Nothing Then
                result &= " = " & value.ToString()
            End If
            Return result

        End If

    End Function

End Class

'==============================
'========== RELATION ==========
'==============================
Public Class RelationNode
    Inherits containerNode

    'Variable
    Public operator_name As token
    Public Arguments As List(Of FunctionArgument)

    Public ReturnType As typeNode = Nothing

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal operator_name As token, ByVal Arguments As List(Of FunctionArgument), ByVal ReturnType As typeNode)
        MyBase.New(positionStart, positionEnd)
        Me.operator_name = operator_name
        Me.Arguments = Arguments
        For Each arg As FunctionArgument In Me.Arguments
            arg.type.parentNode = Me
        Next
        Me.ReturnType = ReturnType
        If Not Me.ReturnType Is Nothing Then
            Me.ReturnType.parentNode = Me
        End If
    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Argument
        Dim argumentsSTR As String = ""
        If Arguments.Count > 0 Then
            For Each arg As FunctionArgument In Arguments
                argumentsSTR &= ", " & arg.ToString()
            Next
            argumentsSTR = argumentsSTR.Substring(2)
        End If

        'Return type
        Dim returnTypeSTR As String = ""
        If ReturnType IsNot Nothing Then
            returnTypeSTR = ":" & Me.ReturnType.ToString()
        End If

        'Return
        Return String.Format("(function {1}({2}){3})", Me.operator_name.ToString(), argumentsSTR, returnTypeSTR)


    End Function

    'Clone
    Public Function clone() As RelationNode
        Return Me.MemberwiseClone()
    End Function

End Class