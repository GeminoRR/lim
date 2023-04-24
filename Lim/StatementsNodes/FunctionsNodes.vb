'==========================
'========== NODE ==========
'==========================
'
' Represents the type of a value.
'
Class FunctionNode
    Inherits ScopeNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReturnTypeNode As TypeNode
    Public ReadOnly FunctionName As String
    Public FunctionArguments As New List(Of FunctionArgumentNode)
    Public Export As Boolean
    Public Codes As New List(Of StatementNode)
    Public ReadOnly CompiledName As String
    Public ReadOnly CustomHeader As String = ""

    Public ReadOnly Property ReturnType As Type
        Get
            If _ReturnType Is Nothing Then
                If ReturnTypeNode Is Nothing Then
                    _ReturnType = Nothing
                Else
                    _ReturnType = ReturnTypeNode.AssociateType
                End If
            End If
            Return _ReturnType
        End Get
    End Property
    Private _ReturnType As Type

    Public ReadOnly Property FunctionType As Type
        Get

            'Already find it
            If _FunctionType IsNot Nothing Then
                Return _FunctionType
            End If

            'Compile
            Me.Compile(Nothing)

            'Create the type arguments
            Dim FunTypeArguments As New List(Of Type) From {Me.ReturnType}
            For Each arg As FunctionArgumentNode In Me.FunctionArguments
                FunTypeArguments.Add(arg.ArgumentType)
            Next

            'Create the type of the function
            _FunctionType = GetTypeFromClassAndArgs(Me, STD_funClass, FunTypeArguments)
            Return _FunctionType

        End Get
    End Property
    Private _FunctionType As Type = Nothing

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal FunctionName As String, ByVal Export As Boolean)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.FunctionName = FunctionName
        Me.Export = Export
        Me.CompiledName = GetFunctionCompiledName()

    End Sub
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal CustomHeader As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.FunctionName = ""
        Me.FunctionArguments = New List(Of FunctionArgumentNode)
        Me.CustomHeader = CustomHeader
        Me.Export = False
        Me.CompiledName = GetFunctionCompiledName()

    End Sub


    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String

        Dim Export_STR As String = ""
        If Export Then
            Export_STR = "export "
        End If

        Dim Arguments_STR As String = ""
        If FunctionArguments.Count > 0 Then
            For Each arg As FunctionArgumentNode In FunctionArguments
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

        Return Export_STR & "func " & FunctionName & Arguments_STR & ReturnType_STR & Content_STR

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

        'Is a method
        Dim ParentType As Type = Nothing
        If TypeOf Me.ParentNode Is Type Then
            ParentType = DirectCast(Me.ParentNode, Type)
        End If

        'HEADER - Return type
        Dim Header As String
        If Me.ReturnType Is Nothing Then
            Header = "void * "
        Else
            Header = Me.ReturnType.CompiledName & " * "
        End If

        'Custom header
        If CustomHeader = "" Then

            'HEADER - Function name
            Header &= Me.CompiledName

            'HEADER - Arguments
            Dim Arguments As String = "void * undefined_self"
            For Each arg As FunctionArgumentNode In Me.FunctionArguments
                Dim ArgumentVariable As New Variable(arg.ArgumentName, arg.ArgumentType)
                Me.Variables.Add(ArgumentVariable)
                Arguments &= ", " & arg.ArgumentType.CompiledName & " * " & ArgumentVariable.CompiledName
            Next
            If Arguments.StartsWith(", ") Then
                Arguments = Arguments.Substring(2)
            End If
            Header &= "(" & Arguments & ")"

        Else

            'HEADER - Custom header
            Header &= Me.CustomHeader

        End If


        'Add header to prototypes
        If ParentType Is Nothing Then
            If CustomHeader = "" Then
                Compiler.Compiled_FunctionsPrototypes.Add("/* " & Me.FunctionName & " */ " & Header & ";")
            Else
                Compiler.Compiled_FunctionsPrototypes.Add("/* <" & Me.ParentFile.filename & "> line " & (Me.PositionStartY + 1).ToString() & " */ " & Header & ";")
            End If
        Else
            Compiler.Compiled_FunctionsPrototypes.Add("/* " & ParentType.ToString() & " -> " & Me.FunctionName & "*/ " & Header & ";")
        End If

        'Content
        Dim FunctionLines As New List(Of String)
        For Each line As StatementNode In Me.Codes
            line.Compile(FunctionLines)
        Next

        'Add header
        Compiler.Compiled_Functions.Add("")
        If Me.CustomHeader = "" Then
            Compiler.Compiled_Functions.Add("//" & Me.FunctionName)
        Else
            Compiler.Compiled_Functions.Add("//Custom Function <" & Me.ParentFile.filename & "> line " & (Me.PositionStartY + 1).ToString())
        End If
        Compiler.Compiled_Functions.Add(Header & "{")

        'Method
        If ParentType IsNot Nothing Then
            Compiler.Compiled_Functions.Add(vbTab & "")
            Compiler.Compiled_Functions.Add(vbTab & "//Null Object pointer")
            Compiler.Compiled_Functions.Add(vbTab & "if (undefined_self == NULL){")
            Compiler.Compiled_Functions.Add(vbTab & vbTab & "ThrowRuntimeError(""The \""" & Me.FunctionName & "\"" method of the \""" & ParentType.ParentClass.ClassName & "\"" class was used on a null object."")")
            Compiler.Compiled_Functions.Add(vbTab & "}")
            Compiler.Compiled_Functions.Add(vbTab & "//Cast struct")
            Compiler.Compiled_Functions.Add(vbTab & ParentType.CompiledName & " * self = (" & ParentType.CompiledName & "*)undefined_self;")
        End If

        'Add content
        For Each line As String In FunctionLines
            Compiler.Compiled_Functions.Add(vbTab & line)
        Next

        'End
        Compiler.Compiled_Functions.Add(vbTab)
        Compiler.Compiled_Functions.Add("}")

    End Sub

    '===========================
    '========== CLONE ==========
    '===========================
    Public Function Clone() As FunctionNode
        Return Me.MemberwiseClone()
    End Function

End Class

'============================================
'========== FUNCTION ARGUMENT NODE ==========
'============================================
'
' Represents the argument of a function
'
Class FunctionArgumentNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ArgumentTypeNode As TypeNode
    Public ArgumentName As String
    Public ArgumentDefaultValue As ValueNode
    Public ReadOnly Property ArgumentType As Type
        Get
            If _ArgumentType Is Nothing Then

                If ArgumentTypeNode IsNot Nothing And ArgumentDefaultValue Is Nothing Then
                    'name:foo
                    _ArgumentType = ArgumentTypeNode.AssociateType

                ElseIf ArgumentTypeNode Is Nothing And ArgumentDefaultValue IsNot Nothing Then
                    'name = bar
                    If Not ArgumentDefaultValue.IsConstant Then
                        ThrowNodeSyntaxException("FNFAN01", "The default value of an argument must be a constant value.", ArgumentDefaultValue)
                    End If
                    _ArgumentType = ArgumentDefaultValue.ReturnType

                ElseIf ArgumentTypeNode IsNot Nothing And ArgumentDefaultValue IsNot Nothing Then
                    'name:foo = bar
                    If Not ArgumentDefaultValue.IsConstant Then
                        ThrowNodeSyntaxException("FNFAN02", "The default value of an argument must be a constant value.", ArgumentDefaultValue)
                    End If
                    If Not ArgumentTypeNode.AssociateType = ArgumentDefaultValue.ReturnType Then
                        ThrowNodeSyntaxException("FNFAN03", "Argument type (" & ArgumentTypeNode.AssociateType.ToString() & ") does not match default value type (<" & ArgumentDefaultValue.ReturnType.ToString() & ">).", ArgumentDefaultValue)
                    End If
                    _ArgumentType = ArgumentTypeNode.AssociateType

                End If

            End If
            Return _ArgumentType
        End Get
    End Property
    Private _ArgumentType As Type

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal ArgumentName As String, ByVal ArgumentTypeNode As TypeNode, ByVal ArgumentDefaultValue As ValueNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.ArgumentName = ArgumentName
        Me.ArgumentTypeNode = ArgumentTypeNode
        If Me.ArgumentTypeNode IsNot Nothing Then
            Me.ArgumentTypeNode.ParentNode = Me
        End If
        Me.ArgumentDefaultValue = ArgumentDefaultValue
        If Me.ArgumentDefaultValue IsNot Nothing Then
            Me.ArgumentDefaultValue.ParentNode = Me
        End If

    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Dim Type_STR As String = ""
        If ArgumentTypeNode IsNot Nothing Then
            Type_STR = ":" & ArgumentTypeNode.ToString()
        End If
        Dim Value_STR As String = ""
        If ArgumentDefaultValue IsNot Nothing Then
            Value_STR = " = (" & ArgumentDefaultValue.ToString() & ")"
        End If
        Return ArgumentName & Type_STR & Value_STR
    End Function

End Class