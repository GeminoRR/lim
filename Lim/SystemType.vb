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
    Public Name As String
    Public arguments As List(Of String)

    Public compiledName As String = ""
    Public given_arguments As New List(Of Type)

    Public compiled As Boolean

    Public variables As New List(Of Variable)
    Public methods As New List(Of FunctionNode)
    Public relations As New List(Of RelationNode)

    Public export As Boolean = False
    Public primary As Boolean = False

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String, ByVal arguments As List(Of String))

        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.compiled = False
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
        Return Me.Name & argumentsSTR

    End Function

    'Equal
    Public Shared Operator =(ByVal type1 As Type, ByVal type2 As Type) As Boolean

        Return type1.compiledName = type2.compiledName

    End Operator

    'Not Equal
    Public Shared Operator <>(ByVal type1 As Type, ByVal type2 As Type) As Boolean

        Return Not type1.compiledName = type2.compiledName

    End Operator

End Class