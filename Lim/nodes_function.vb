﻿'==============================
'========== FUNCTION ==========
'==============================
Public Class FunctionNode
    Inherits containerNode

    'Variable
    Public Name As String
    Public Arguments As List(Of FunctionArgument)
    Public maxArguments As Integer
    Public minArguments As Integer

    Public unsafeReturnType As typeNode = Nothing
    Public ReturnType As safeType = Nothing

    Public compiledName As String = ""
    Public compiledID As Integer = 0
    Public compiled As Boolean
    Public compiling As Boolean

    Public export As Boolean = False

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String, ByVal Arguments As List(Of FunctionArgument), ByVal unsafeReturnType As typeNode)
        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.Arguments = Arguments
        Me.maxArguments = Me.Arguments.Count
        Me.minArguments = 0
        Dim lastArgWasOptional As Boolean = False
        For Each arg As FunctionArgument In Me.Arguments
            arg.type.parentNode = Me
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
        If Not Me.unsafeReturnType Is Nothing Then
            Me.unsafeReturnType.parentNode = Me
        End If
        Me.compiled = False
        Me.compiling = False
        Me.compiledID = get_new_id()
    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Unsafe type
        Dim UST As String = ""
        If Not unsafeReturnType Is Nothing Then
            UST = ":" & unsafeReturnType.ToString()
        End If

        'Argument
        Dim ATS As String = ""
        If Arguments.Count > 0 Then
            For Each arg As FunctionArgument In Arguments
                ATS &= ", " & arg.ToString()
            Next
            ATS = ATS.Substring(2)
        End If

        'Actions
        Dim LTS As String = " = *" & Me.codes.Count.ToString() & " elements*"

        'Return
        Return "(" & Name & "(" & ATS & ")" & UST & LTS & ")"

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
            Return name & ":" & type.ToString()

        Else

            'Copy
            Return "var " & name & ":" & type.ToString()

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

    Public unsafeReturnType As typeNode = Nothing
    Public ReturnType As safeType = Nothing

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal operator_name As token, ByVal Arguments As List(Of FunctionArgument), ByVal unsafeReturnType As typeNode)
        MyBase.New(positionStart, positionEnd)
        Me.operator_name = operator_name
        Me.Arguments = Arguments
        For Each arg As FunctionArgument In Me.Arguments
            arg.type.parentNode = Me
        Next
        Me.unsafeReturnType = unsafeReturnType
        If Not Me.unsafeReturnType Is Nothing Then
            Me.unsafeReturnType.parentNode = Me
        End If
    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Unsafe type
        Dim UST As String = ""
        If Not unsafeReturnType Is Nothing Then
            UST = ":" & unsafeReturnType.ToString()
        End If

        'Argument
        Dim ATS As String = ""
        If Arguments.Count > 0 Then
            For Each arg As FunctionArgument In Arguments
                ATS &= ", " & arg.ToString()
            Next
            ATS = ATS.Substring(2)
        End If

        'Actions
        Dim LTS As String = " = *" & Me.codes.Count.ToString() & " elements*"

        'Return
        Return "(relation " & operator_name.ToString() & "(" & ATS & ")" & UST & LTS & ")"

    End Function

End Class