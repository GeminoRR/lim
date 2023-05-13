'===============================
'========== TYPE NODE ==========
'===============================
'
' Represents a type node
'
Class TypeNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ClassName As String
    Public PassedArguments As New List(Of TypeNode)
    Public ReadOnly Property AssociateType As Type
        Get
            If _AssociateType Is Nothing Then

                'Generic Type
                If PassedArguments.Count = 0 Then
                    Dim ParentNode As Node = Me
                    While ParentNode.ParentNode IsNot Nothing

                        ParentNode = ParentNode.ParentNode
                        If Not TypeOf ParentNode Is Type Then
                            Continue While
                        End If

                        Dim ParentType As Type = DirectCast(ParentNode, Type)
                        For i As Integer = 0 To ParentType.ParentClass.Arguments.Count - 1
                            If ParentType.ParentClass.Arguments(i) = ClassName Then
                                _AssociateType = ParentType.PassedArguments(i)
                                Return _AssociateType
                            End If
                        Next

                        Exit While

                    End While
                End If

                'Search existing type
                For Each AlreadyCompiledType As Type In Compiler.DefinedTypes

                    'Not the same type
                    If Me.IsTheSameAs(AlreadyCompiledType) Then
                        _AssociateType = AlreadyCompiledType
                        Return _AssociateType
                    End If

                Next

                'Type not already compiled, do so
                Dim CompiledArguments As New List(Of Type)
                For Each arg As TypeNode In Me.PassedArguments
                    If arg Is Nothing Then
                        CompiledArguments.Add(Nothing)
                    Else
                        CompiledArguments.Add(arg.AssociateType)
                    End If
                Next
                Dim NewType As New Type(Me.AssociateClassNode, CompiledArguments)
                _AssociateType = NewType
                Return _AssociateType

            End If
            Return _AssociateType
        End Get
    End Property
    Private _AssociateType As Type = Nothing

    Private ReadOnly Property AssociateClassNode As ClassNode
        Get
            If _AssociateClassNode Is Nothing Then

                'Generic Type
                If PassedArguments.Count = 0 Then
                    Dim ParentNode As Node = Me
                    While ParentNode.ParentNode IsNot Nothing

                        ParentNode = ParentNode.ParentNode
                        If Not TypeOf ParentNode Is Type Then
                            Continue While
                        End If

                        Dim ParentType As Type = DirectCast(ParentNode, Type)
                        For i As Integer = 0 To ParentType.ParentClass.Arguments.Count - 1
                            If ParentType.ParentClass.Arguments(i) = ClassName Then
                                _AssociateClassNode = ParentType.PassedArguments(i).ParentClass
                                Return _AssociateClassNode
                            End If
                        Next

                        Exit While

                    End While
                End If

                'Search in class of current file
                For Each ClassNode As ClassNode In Me.ParentFile.Classes
                    If ClassNode.ClassName = Me.ClassName Then
                        _AssociateClassNode = ClassNode
                        Return _AssociateClassNode
                    End If
                Next

                'Search in other files
                For Each ImportedFile As SourceFile In Me.ParentFile.ImportedFiles
                    For Each ClassNode As ClassNode In ImportedFile.Classes
                        If ClassNode.Export And ClassNode.ClassName = ClassName Then
                            _AssociateClassNode = ClassNode
                            Return _AssociateClassNode
                        End If
                    Next
                Next

                'Not find
                ThrowNodeTypeException("TNACN01", "The """ & Me.ToString() & """ type is undefined.", Me)

            End If
            Return _AssociateClassNode
        End Get
    End Property
    Private _AssociateClassNode As ClassNode = Nothing

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As TypeNode = Me.MemberwiseClone()
        For i As Integer = 0 To Cloned.PassedArguments.Count - 1
            Cloned.PassedArguments(i) = Cloned.PassedArguments(i).Clone(Cloned)
        Next
        Cloned._AssociateClassNode = Nothing
        Cloned._AssociateType = Nothing
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal ClassName As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.ClassName = ClassName

        'Fix fun
        If ClassName = "fun" Then

        End If

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Dim Arguments_STR As String = ""
        If PassedArguments.Count > 0 Then
            For Each arg As TypeNode In PassedArguments
                If arg Is Nothing Then
                    Arguments_STR &= ", Nothing"
                Else
                    Arguments_STR &= ", " & arg.ToString() & ""
                End If
            Next
            Arguments_STR = "<" & Arguments_STR.Substring(2) & ">"
        End If
        Return ClassName & Arguments_STR
    End Function

    '===========================
    '========== EQUAL ==========
    '===========================
    Private Function IsTheSameAs(ByVal T As Type) As Boolean

        'Same class
        If Not Me.AssociateClassNode = T.ParentClass Then
            Return False
        End If

        'Arguments count
        If Not Me.PassedArguments.Count = T.PassedArguments.Count Then
            Return False
        End If

        'Arguments
        For i As Integer = 0 To Me.PassedArguments.Count - 1
            If Me.PassedArguments(i) Is Nothing Then
                If T.PassedArguments(i) IsNot Nothing Then
                    Return False
                End If
            Else
                If Not Me.PassedArguments(i).IsTheSameAs(T.PassedArguments(i)) Then
                    Return False
                End If
            End If
        Next

        'The sames
        Return True

    End Function

End Class