'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Class RelationNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReturnTypeNode As TypeNode
    Public RelationOperator As RelationOperator
    Public RelationArguments As New List(Of FunctionArgumentNode)
    Public Codes As New List(Of Node)
    Public CompiledName As String
    Public Property ReturnType As Type
        Get
            If _ReturnType Is Nothing Then
                If ReturnTypeNode Is Nothing Then
                    _ReturnType = Nothing
                Else
                    _ReturnType = ReturnTypeNode.AssociateType
                End If
                Compile(Nothing)
            End If
            Return _ReturnType
        End Get
        Set(value As Type)

            'No explicit type
            If _ReturnType Is Nothing Then
                _ReturnType = value
            End If

            'Check if newtype is the same as the older
            If Not value = _ReturnType Then
                ThrowNodeTypeException("RNRT01", "The instructions of this relation do not agree on the type of the value to be returned.", Me)
            End If

            'Set
            _ReturnType = value

        End Set
    End Property
    Private _ReturnType As Type

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As RelationNode = Me.MemberwiseClone()
        If Cloned.ReturnTypeNode IsNot Nothing Then
            Cloned.ReturnTypeNode = Cloned.ReturnTypeNode.Clone(Cloned)
        End If
        Cloned.CompiledName = GetRelationCompiledName()
        Cloned._ReturnType = Nothing
        Cloned.Compiled = False
        For i As Integer = 0 To Cloned.RelationArguments.Count - 1
            Cloned.RelationArguments(i) = Cloned.RelationArguments(i).Clone(Cloned)
        Next
        For i As Integer = 0 To Cloned.Codes.Count - 1
            Cloned.Codes(i) = Cloned.Codes(i).Clone(Cloned)
        Next
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal RelationOperator As RelationOperator, ByVal Arguments As List(Of FunctionArgumentNode))

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.RelationOperator = RelationOperator
        Me.RelationArguments = Arguments
        For Each arg As FunctionArgumentNode In Me.RelationArguments
            arg.ParentNode = Me
        Next
        Me.CompiledName = GetRelationCompiledName()

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim Arguments_STR As String = ""
        If RelationArguments.Count > 0 Then
            For Each arg As FunctionArgumentNode In RelationArguments
                Arguments_STR &= ", " & arg.ToString()
            Next
            Arguments_STR = "(" & Arguments_STR.Substring(2) & ")"
        End If

        Dim ReturnType_STR As String = ""
        If ReturnTypeNode IsNot Nothing Then
            ReturnType_STR = ":" & ReturnTypeNode.ToString()
        End If

        Dim Content_STR As String = ""
        If Codes.Count > 0 Then
            For Each content As Node In Codes
                Content_STR &= Environment.NewLine & content.ToString()
            Next
            Content_STR = Environment.NewLine & "(" & Content_STR & Environment.NewLine & ")"
        End If

        Return "relation " & RelationOperator.ToString() & Arguments_STR & ReturnType_STR & Content_STR

    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Private Compiled As Boolean = False
    Public Overrides Sub Compile(ByVal Content As List(Of String))

        'Compiled
        If Compiled Then
            Exit Sub
        End If
        Compiled = True

        'Verify first arg type
        If Not RelationArguments(0).ArgumentType = DirectCast(Me.ParentNode, Type) Then
            ThrowNodeTypeException("RNC01", "The first argument of a relation must necessarily be the class of the relation.", RelationArguments(0))
        End If

        ' Argument
        ' Code (for return type)
        ' Return type
        ' Header
        ' Core

        'HEADER - Arguments
        Dim FunctionLines As New List(Of String)
        Dim Arguments As String = ""
        For Each arg As FunctionArgumentNode In Me.RelationArguments

            'Add argument to header
            Dim ArgumentVariable As New Variable(arg.ArgumentName, arg.ArgumentType)
            Me.Variables.Add(ArgumentVariable)
            Arguments &= ", " & arg.ArgumentType.CompiledName & " * " & ArgumentVariable.CompiledName

        Next
        If Arguments.StartsWith(", ") Then
            Arguments = Arguments.Substring(2)
        End If
        Dim Header As String = Me.CompiledName & "(" & Arguments & ")"

        'Compile all code
        For Each line As StatementNode In Me.Codes
            line.Compile(FunctionLines)
        Next

        'HEADER - Return Type
        If Me.ReturnType Is Nothing Then
            ThrowNodeTypeException("RNC02", "A relation must absolutely return a value, which is not the case here.", Me)
        End If
        Header = Me.ReturnType.CompiledName & " * " & Header

        'Add header to prototypes
        Compiler.Compiled_FunctionsPrototypes.Add("/* " & DirectCast(Me.ParentNode, Type).ParentClass.ClassName & " -> Relation " & Me.RelationOperator.ToString() & " */ " & Header & ";")

        'Add header
        Compiler.Compiled_Functions.Add("")
        Compiler.Compiled_Functions.Add("// " & DirectCast(Me.ParentNode, Type).ParentClass.ClassName & " -> Relation " & Me.RelationOperator.ToString())
        Compiler.Compiled_Functions.Add(Header & "{")

        'Add content
        For Each line As String In FunctionLines
            Compiler.Compiled_Functions.Add(vbTab & line)
        Next

        'Return
        Compiler.Compiled_Functions.Add(vbTab & "")
        Compiler.Compiled_Functions.Add(vbTab & "//Return")
        Compiler.Compiled_Functions.Add(vbTab & "return NULL;")

        'End
        Compiler.Compiled_Functions.Add(vbTab)
        Compiler.Compiled_Functions.Add("}")
    End Sub

End Class

'=======================================
'========== RELATION OPERATOR ==========
'=======================================
Enum RelationOperator

    PLUS
    MINUS
    MULTIPLICATION
    DIVISION

    EQUAL
    LESSTHAN
    LESSTHANEQUAL
    MORETHAN
    MORETHANEQUAL

    INDEX

End Enum