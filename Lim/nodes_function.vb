'==============================
'========== FUNCTION ==========
'==============================
Public Class FunctionNode
    Inherits containerNode

    'Variable
    Public Name As String
    Public Arguments As List(Of FunctionArgument)

    Public unsafeReturnType As typeNode = Nothing
    Public ReturnType As safeType = Nothing

    Public compiledName As String = ""
    Public compiled As Boolean
    Public compiling As Boolean

    Public export As Boolean = False

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String, ByVal Arguments As List(Of FunctionArgument), ByVal unsafeReturnType As typeNode)
        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.Arguments = Arguments
        For Each arg As FunctionArgument In Me.Arguments
            arg.type.parentNode = Me
        Next
        Me.unsafeReturnType = unsafeReturnType
        If Not Me.unsafeReturnType Is Nothing Then
            Me.unsafeReturnType.parentNode = Me
        End If
        Me.compiled = False
        Me.compiling = False
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
    Public Sub New(ByVal name As String, ByVal type As typeNode, ByVal declareType As VariableDeclarationType)
        Me.name = name
        Me.type = type
        Me.declareType = declareType
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