'=================================
'========== UNSAFE TYPE ==========
'=================================
Public Class typeNode
    Inherits Node

    Public className As String
    Public Dimensions As New List(Of ValueType)

    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal className As String, ByVal Dimensions As List(Of ValueType))
        MyBase.New(positionStart, positionEnd)
        Me.className = className
        If Not Dimensions Is Nothing Then
            For Each Dimension As ValueType In Dimensions
                Me.Dimensions.Add(Dimension)
            Next
        End If
    End Sub

    Public Overrides Function ToString() As String
        Dim str As String = ""
        For Each dimension As ValueType In Dimensions
            Select Case dimension.ToString()
                Case "list"
                    str &= "[]"
                Case "map"
                    str &= "{}"
                Case Else
                    str &= "?"
            End Select
        Next
        Return className.ToString() & str
    End Function

    Public Function getParentType() As typeNode
        Dim parentDimension As New List(Of ValueType)
        For Each dimension As ValueType In Me.Dimensions
            parentDimension.Add(dimension)
        Next
        If parentDimension.Count > 0 Then
            parentDimension.RemoveAt(parentDimension.Count - 1)
        Else
            addBasicError("Cannot resolve the type", "Cannot get parent type of a simple type")
        End If
        Dim returnType As New typeNode(Me.positionStart, Me.positionEnd - 2, Me.className, parentDimension)
        returnType.parentNode = Me
        Return returnType
    End Function

End Class

'================================
'========== VALUE TYPE ==========
'================================
Public Enum ValueType
    list
    map
End Enum

'===============================
'========== SAVE TYPE ==========
'===============================
Public Class safeType

    Public TargetClass As ClassNode
    Public Dimensions As New List(Of ValueType)

    Public Sub New(ByVal Struct As ClassNode, Optional ByVal Dimensions As List(Of ValueType) = Nothing)
        Me.TargetClass = Struct
        If Not Dimensions Is Nothing Then
            For Each Dimension As ValueType In Dimensions
                Me.Dimensions.Add(Dimension)
            Next
        End If
    End Sub

    Public Overrides Function ToString() As String
        Dim str As String = ""
        For Each dimension As ValueType In Dimensions
            Select Case dimension.ToString()
                Case "list"
                    str &= "[]"
                Case "map"
                    str &= "{}"
                Case Else
                    str &= "?"
            End Select
        Next
        Return TargetClass.Name & str
    End Function

    Public Function getParentType() As safeType
        Dim parentDimension As New List(Of ValueType)
        For Each dimension As ValueType In Me.Dimensions
            parentDimension.Add(dimension)
        Next
        If parentDimension.Count > 0 Then
            parentDimension.RemoveAt(parentDimension.Count - 1)
        End If
        Return New safeType(Me.TargetClass, parentDimension)
    End Function

    Public Function IsTheSameAs(ByVal other As safeType) As Boolean

        'Name
        If Not Me.TargetClass.compiledName = other.TargetClass.compiledName Then
            Return False
        End If

        'Dimensions
        If Not Me.Dimensions.Count = other.Dimensions.Count Then
            Return False
        End If
        For i As Integer = 0 To Me.Dimensions.Count - 1
            If Not Me.Dimensions(i) = other.Dimensions(i) Then
                Return False
            End If
        Next

        'Pass all checks
        Return True

    End Function

    Public Function clone() As safeType
        Dim dims As New List(Of ValueType)
        For Each dimension As ValueType In Dimensions
            dims.Add(dimension)
        Next
        Return New safeType(TargetClass, dims)
    End Function

End Class