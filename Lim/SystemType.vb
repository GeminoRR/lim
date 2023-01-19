'===============================
'========== TYPE NODE ==========
'===============================
Public Class typeNode
    Inherits Node

    Public className As String
    Public arguments As List(Of typeNode)

    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal className As String, ByVal arguments As List(Of typeNode))
        MyBase.New(positionStart, positionEnd)
        Me.className = className
        Me.arguments = arguments
    End Sub

    Public Overrides Function ToString() As String
        Dim argumentsStr As String = ""
        For Each arg As typeNode In arguments
            argumentsStr &= ", " & arg.ToString()
        Next
        If argumentsStr.StartsWith(", ") Then
            argumentsStr = "<" & argumentsStr.Substring(2) & ">"
        End If
        Return className.ToString() & argumentsStr
    End Function

End Class

'==========================
'========== TYPE ==========
'==========================
Public Class Type
    Inherits Node

    'Variable
    Public target As ClassNode
    Public arguments As List(Of String)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal target As ClassNode, ByVal arguments As List(Of String))

        MyBase.New(positionStart, positionEnd)
        Me.target = target
        Me.arguments = arguments

    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Arguments
        Dim argumentsSTR As String = ""
        For Each arg As String In Me.arguments
            argumentsSTR &= ", " & arg
        Next
        If argumentsSTR.StartsWith(", ") Then
            argumentsSTR = "<" & argumentsSTR.Substring(2) & ">"
        End If

        'Return
        Return Me.target.Name & argumentsSTR

    End Function

    'Equal
    Public Shared Operator =(ByVal type1 As Type, ByVal type2 As Type) As Boolean

        'Argument count
        If Not type1.arguments.Count = type2.arguments.Count Then
            Return False
        End If

        'Same class
        If Not type1.target.compiledName = type2.target.compiledName Then
            Return False
        End If

        'Same argument
        For i As Integer = 0 To type1.arguments.Count - 1
            If Not type1.arguments(i) = type2.arguments(i) Then
                Return False
            End If
        Next

        'Ok
        Return True

    End Operator

    'Not Equal
    Public Shared Operator <>(ByVal type1 As Type, ByVal type2 As Type) As Boolean

        Return Not type1 = type2

    End Operator

End Class