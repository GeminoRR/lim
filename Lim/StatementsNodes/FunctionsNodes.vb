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
    Public FunctionName As String
    Public FunctionArguments As New List(Of FunctionArgumentNode)
    Public Export As Boolean
    Public Codes As New List(Of StatementNode)
    Public CompiledName As String
    Public CustomHeader As String = ""
    Public MinArguments As Integer
    Public MaxArguments As Integer

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As FunctionNode = Me.MemberwiseClone()
        If Cloned.ReturnTypeNode IsNot Nothing Then
            Cloned.ReturnTypeNode = Cloned.ReturnTypeNode.Clone(Cloned)
        End If
        Cloned.CompiledName = GetRelationCompiledName()
        Cloned._ReturnType = Nothing
        Cloned.Compiled = False
        Cloned.FunctionArguments = New List(Of FunctionArgumentNode)
        For i As Integer = 0 To Me.FunctionArguments.Count - 1
            Cloned.FunctionArguments.Add(Me.FunctionArguments(i).Clone(Cloned))
        Next
        Cloned.Codes = New List(Of StatementNode)
        For i As Integer = 0 To Me.Codes.Count - 1
            Cloned.Codes.Add(Me.Codes(i).Clone(Cloned))
        Next
        Return Cloned

    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
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
                ThrowNodeTypeException("FNRT01", "The instructions of this function do not agree on the type of the value to be returned.", Me)
            End If

            'Set
            _ReturnType = value

        End Set
    End Property
    Private _ReturnType As Type

    '===================================
    '========== FUNCTION TYPE ==========
    '===================================
    Public ReadOnly Property MinimumFunctionType As Type
        Get

            'Already find it
            If _FunctionType IsNot Nothing Then
                Return _FunctionType
            End If

            'Compile
            Me.Compile(Nothing)

            'Create the type arguments
            Dim FunTypeArguments As New List(Of Type) From {Me.ReturnType}
            For i As Integer = 0 To Me.MinArguments - 1
                FunTypeArguments.Add(Me.FunctionArguments(i).ArgumentType)
            Next

            'Create the type of the function
            _FunctionType = GetTypeFromClassAndArgs(Me, STDClass_fun, FunTypeArguments)
            Return _FunctionType

        End Get
    End Property
    Private _FunctionType As Type = Nothing

    '==============================================
    '========== COMPILE MINIMUM FUNCTION ==========
    '==============================================
    Public ReadOnly Property MinimumFunctionCompiledName As String
        Get

            'Already find it
            If _MminimumFunctionCompiledName IsNot Nothing Then
                Return _MminimumFunctionCompiledName
            End If

            'Compile transition function (for optional arguments)
            Dim TargetValue As String = Me.CompiledName
            If Me.MaxArguments > MinArguments Then

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

                'HEADER - Function name
                TargetValue = "MinimumArgCountOf_" & Me.CompiledName
                Header &= TargetValue

                'HEADER - Arguments & Content
                Dim CallArguments As String = "undefined_self"
                Dim Arguments As String = "void * undefined_self"
                For i As Integer = 0 To MaxArguments - 1
                    If i >= MinArguments Then

                        'Optionnal argument
                        CallArguments &= ", NULL"

                    Else

                        'Mandatory argument
                        Dim ArgCompiledName As String = GetVariableCompiledName()
                        Arguments &= ", " & Me.FunctionArguments(i).ArgumentType.CompiledName & " * " & ArgCompiledName
                        CallArguments &= ", " & ArgCompiledName

                    End If

                Next
                Header &= "(" & Arguments & ")"

                'Add header to prototypes
                If ParentType Is Nothing Then
                    Compiler.Compiled_FunctionsPrototypes.Add("/* " & Me.FunctionName & " (without optional argument) */ " & Header & ";")
                Else
                    Compiler.Compiled_FunctionsPrototypes.Add("/* " & ParentType.ToString() & " -> " & Me.FunctionName & " (without optional argument) */ " & Header & ";")
                End If

                'Compile function
                Compiler.Compiled_Functions.Add("")
                If ParentType Is Nothing Then
                    Compiler.Compiled_Functions.Add("// " & Me.FunctionName & " (without optional argument)")
                Else
                    Compiler.Compiled_Functions.Add("// " & ParentType.ToString() & " -> " & Me.FunctionName & " (without optional argument)")
                End If
                Compiler.Compiled_Functions.Add(Header & "{")
                Compiler.Compiled_Functions.Add(vbTab & Me.CompiledName & "(" & CallArguments & ");")
                Compiler.Compiled_Functions.Add("}")

            End If

            'Return
            _MminimumFunctionCompiledName = TargetValue
            Return _MminimumFunctionCompiledName

        End Get
    End Property
    Private _MminimumFunctionCompiledName As String = Nothing

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal FunctionName As String, ByVal Export As Boolean, ByVal Arguments As List(Of FunctionArgumentNode), ByVal ReturnType As TypeNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.FunctionName = FunctionName
        Me.Export = Export
        Me.ReturnTypeNode = ReturnType
        If Me.ReturnTypeNode IsNot Nothing Then
            Me.ReturnTypeNode.ParentNode = Me
        End If
        Me.FunctionArguments = Arguments
        Me.MaxArguments = Me.FunctionArguments.Count
        Me.MinArguments = 0
        For Each arg As FunctionArgumentNode In Me.FunctionArguments
            arg.ParentNode = Me
            If arg.ArgumentDefaultValue Is Nothing Then
                Me.MinArguments += 1
            End If
        Next
        Me.CompiledName = GetFunctionCompiledName()

    End Sub
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal CustomHeader As String, ByVal ReturnType As TypeNode)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.FunctionName = ""
        Me.FunctionArguments = New List(Of FunctionArgumentNode)
        Me.CustomHeader = CustomHeader
        Me.Export = False
        Me.ReturnTypeNode = ReturnType
        If Me.ReturnTypeNode IsNot Nothing Then
            Me.ReturnTypeNode.ParentNode = Me
        End If
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
    Public Compiled As Boolean = False
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

        ' Argument
        ' Code (for return type)
        ' Return type
        ' Header
        ' Core

        'Argument
        Dim Header As String
        Dim FunctionLines As New List(Of String)
        If CustomHeader = "" Then

            'HEADER - Function name
            Header = Me.CompiledName

            'Optionnal arguments comment
            If Not MinArguments = MaxArguments Then

                'Comment
                FunctionLines.Add("")
                FunctionLines.Add("// Value of optional arguments")

            End If

            'HEADER - Arguments
            Dim Arguments As String = If(ParentType IsNot Nothing And FunctionName = "new", "", "void * undefined_self")
            For Each arg As FunctionArgumentNode In Me.FunctionArguments

                'Add argument to header
                Dim ArgumentVariable As New Variable(arg.ArgumentName, arg.ArgumentType)
                Me.Variables.Add(ArgumentVariable)
                Arguments &= ", " & arg.ArgumentType.CompiledName & " * " & ArgumentVariable.CompiledName

                'Default value for optional argument
                If arg.ArgumentDefaultValue IsNot Nothing Then

                    If Not arg.ArgumentDefaultValue.IsConstant Then
                        ThrowNodeTypeException("FNC01", "The default value of an argument must be a constant.", Me)
                    End If

                    Dim IfContent As New List(Of String)
                    FunctionLines.Add("if (" & ArgumentVariable.CompiledName & " == NULL){")
                    IfContent.Add(ArgumentVariable.CompiledName & " = " & arg.ArgumentDefaultValue.Compile(IfContent) & ";")
                    For Each line As String In IfContent
                        FunctionLines.Add(vbTab & line)
                    Next
                    FunctionLines.Add("}")

                End If

            Next
            If Arguments.StartsWith(", ") Then
                Arguments = Arguments.Substring(2)
            End If
            Header &= "(" & Arguments & ")"

        Else

            'Custom header
            Header = CustomHeader

        End If

        'Compile all code
        For Each line As StatementNode In Me.Codes
            line.Compile(FunctionLines)
        Next

        'HEADER - Special function
        Dim CompiledReturnType As String = ""
        If ParentType IsNot Nothing Then

            Select Case FunctionName

                Case "new" 'Constructor
                    If Me.ReturnType IsNot Nothing Then
                        ThrowNodeSyntaxException("FNC02", "The constructor of a class cannot return a value.", Me.ReturnTypeNode)
                    End If
                    FunctionLines.Insert(0, ParentType.CompiledName & " * self = " & ParentType.CompiledName & "_allocate();")
                    FunctionLines.Insert(0, "// Allocate memory")
                    FunctionLines.Insert(0, "")
                    CompiledReturnType = ParentType.CompiledName & " * "

                Case "str"
                    If Me.FunctionArguments.Count > 0 Then
                        ThrowNodeSyntaxException("FNC04", "The ""str"" method cannot take any arguments.", Me)
                    End If
                    If Not Me.ReturnType = STD_str Then
                        If Me.ReturnType Is Nothing Then
                            ThrowNodeSyntaxException("FNC04", "The ""str"" method must necessarily return a character string, but here it returns nothing.", Me)
                        End If
                        ThrowNodeSyntaxException("FNC03", "The ""str"" method must return a character string, but here it returns a value of type (" & Me.ReturnType.ToString() & ").", Me.ReturnTypeNode)
                    End If
                    CompiledReturnType = Me.ReturnType.CompiledName & " * "

                Case "repr"
                    If Me.FunctionArguments.Count > 0 Then
                        ThrowNodeSyntaxException("FNC04", "The ""repr"" method cannot take any arguments.", Me)
                    End If
                    If Not Me.ReturnType = STD_str Then
                        If Me.ReturnType Is Nothing Then
                            ThrowNodeSyntaxException("FNC06", "The ""repr"" method must necessarily return a character string, but here it returns nothing.", Me)
                        End If
                        ThrowNodeSyntaxException("FNC06", "The ""reprrepr"" method must return a character string, but here it returns a value of type (" & Me.ReturnType.ToString() & ").", Me.ReturnTypeNode)
                    End If
                    CompiledReturnType = Me.ReturnType.CompiledName & " * "

                Case "clone"
                    If Me.FunctionArguments.Count > 0 Then
                        ThrowNodeSyntaxException("FNC04", "The ""clone"" method cannot take any arguments.", Me)
                    End If
                    If Not Me.ReturnType = ParentType Then
                        If Me.ReturnType Is Nothing Then
                            ThrowNodeSyntaxException("FNC05", "The ""clone"" method must return an instance of the class in which it is defined.", Me, "The method must return a value of type " & ParentType.ToString() & "instead of nothing.")
                        Else
                            ThrowNodeSyntaxException("FNC05", "The ""clone"" method must return an instance of the class in which it is defined.", Me, "The method must return a value of type " & ParentType.ToString() & "instead of " & Me.ReturnType.ToString() & ".")
                        End If
                    End If
                    If Not FunctionArguments.Count = 0 Then
                        ThrowNodeSyntaxException("FNC05", "The ""clone"" method cannot have parameters.", Me)
                    End If

            End Select

        End If

        'HEADER - Return Type
        If CompiledReturnType = "" Then
            If Me.ReturnType Is Nothing Then
                CompiledReturnType = "void * "
            Else
                CompiledReturnType = Me.ReturnType.CompiledName & " * "
            End If
        End If
        Header = CompiledReturnType & Header

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

        'Add header
        Compiler.Compiled_Functions.Add("")
        If Me.CustomHeader = "" Then
            If ParentType Is Nothing Then
                Compiler.Compiled_Functions.Add("//" & Me.FunctionName)
            Else
                Compiler.Compiled_Functions.Add("//" & ParentType.ToString() & " -> " & Me.FunctionName)
            End If
        Else
            Compiler.Compiled_Functions.Add("//Custom Function <" & Me.ParentFile.filename & "> line " & (Me.PositionStartY + 1).ToString())
        End If
        Compiler.Compiled_Functions.Add(Header & "{")

        'Method
        If ParentType IsNot Nothing And Not FunctionName = "new" Then
            Compiler.Compiled_Functions.Add(vbTab & "")
            Compiler.Compiled_Functions.Add(vbTab & "//Null Object pointer")
            Compiler.Compiled_Functions.Add(vbTab & "if (undefined_self == NULL){")
            Compiler.Compiled_Functions.Add(vbTab & vbTab & "ThrowRuntimeError(""The \""" & Me.FunctionName & "\"" method of the \""" & ParentType.ParentClass.ClassName & "\"" class was used on a null object."");")
            Compiler.Compiled_Functions.Add(vbTab & "}")
            Compiler.Compiled_Functions.Add(vbTab & "")
            Compiler.Compiled_Functions.Add(vbTab & "//Cast struct")
            Compiler.Compiled_Functions.Add(vbTab & ParentType.CompiledName & " * self = (" & ParentType.CompiledName & "*)undefined_self;")
        End If

        'Add content
        For Each line As String In FunctionLines
            Compiler.Compiled_Functions.Add(vbTab & line)
        Next

        'Return
        If ParentType IsNot Nothing And FunctionName = "new" Then
            Compiler.Compiled_Functions.Add(vbTab & "")
            Compiler.Compiled_Functions.Add(vbTab & "//Return self")
            Compiler.Compiled_Functions.Add(vbTab & "return self;")
        Else
            Compiler.Compiled_Functions.Add(vbTab & "")
            Compiler.Compiled_Functions.Add(vbTab & "//Return NULL")
            Compiler.Compiled_Functions.Add(vbTab & "return NULL;")
        End If

        'End
        Compiler.Compiled_Functions.Add(vbTab)
        Compiler.Compiled_Functions.Add("}")

    End Sub

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
    Private _ArgumentType As Type = Nothing

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As FunctionArgumentNode = Me.MemberwiseClone()
        If Cloned.ArgumentTypeNode IsNot Nothing Then
            Cloned.ArgumentTypeNode = Cloned.ArgumentTypeNode.Clone(Cloned)
        End If
        If Cloned.ArgumentDefaultValue IsNot Nothing Then
            Cloned.ArgumentDefaultValue = Cloned.ArgumentDefaultValue.Clone(Cloned)
        End If
        Cloned._ArgumentType = Nothing
        Return Cloned

    End Function

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