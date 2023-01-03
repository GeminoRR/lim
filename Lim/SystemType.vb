'===============================
'========== TYPE NODE ==========
'===============================
Public Class typeNode
    Inherits Node

    Public className As String
    Public targetClass As ClassNode
    Public arguments As List(Of typeNode)

    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal className As String, ByVal arguments As List(Of typeNode))
        MyBase.New(positionStart, positionEnd)
        Me.className = className
        Me.arguments = arguments
        Me.targetClass = Nothing
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

    Public Shared Operator =(ByVal t1 As typeNode, ByVal t2 As typeNode)

        'Fix targetClass
        If t1.targetClass Is Nothing Or t2.targetClass Is Nothing Then
            Dim parentCompiler As C_Compiler = getNodeParentFile(t1).compiler
            If t1.targetClass Is Nothing Then
                t1.targetClass = parentCompiler.getClass(t1.className, t1)
            End If
            If t2.targetClass Is Nothing Then
                t2.targetClass = parentCompiler.getClass(t2.className, t2)
            End If
        End If

        'Compare
        If Not t1.targetClass.compiledName = t2.targetClass.compiledName Then

        End If


    End Operator

    Public Shared Operator <>(ByVal t1 As typeNode, ByVal t2 As typeNode)
        Return True
    End Operator

End Class