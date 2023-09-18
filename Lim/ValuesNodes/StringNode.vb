'========================================
'========== NUMERIC VALUE NODE ==========
'========================================
'
' Represents a character string
'
Imports System.ComponentModel.DataAnnotations
Imports System.Globalization
Imports System.Runtime.CompilerServices

Class StringNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Value As String
    Private Formated As Boolean

    '===============================
    '========== DUPLICATE ==========
    '===============================
    Protected Overrides Function Duplicate() As Node

        Dim Cloned As StringNode = Me.MemberwiseClone()
        Return Cloned

    End Function

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Value As String, Optional ByVal Formated As Boolean = False)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Value = Value
        Me.Formated = Formated

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "'" & Value & "'"
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Get string value
        Dim ProcessValue As String = Me.Value

        'Not formated
        If Not Formated Then

            'Fix string
            ProcessValue = ProcessValue.Replace("\", "\\")
            ProcessValue = ProcessValue.Replace("""", "\""")

            'Return value
            Return "new_str(""" & ProcessValue & """)"

        End If

        'Parse
        Dim ResultValue As String = ""
        Dim WorkingString As String = ProcessValue
        Dim LastGet As String = ""
        Dim Gettings As Boolean = False
        Dim Fvariables As New List(Of String)

        While WorkingString.Length > 0

            If Gettings Then

                LastGet &= WorkingString(0)
                WorkingString = WorkingString.Substring(1)

                If LastGet.EndsWith("}") Then

                    LastGet = LastGet.Substring(0, LastGet.Length - 1)

                    Dim Tokens As List(Of Token) = Lexer.LexLines({LastGet}.ToList(), Me.ParentFile, Me.PositionStartY, Me.PositionStartX)
                    Tokens.RemoveAt(0) 'Remove lineIndentation

                    Dim node As Node = New AST().ParseValue(Tokens, Me.ParentFile)
                    node.ParentNode = Me

                    'Not a value
                    If Not TypeOf node Is ValueNode Then
                        ThrowNodeSyntaxException("SNC01", "This type of syntax is not supported.", node)
                    End If

                    'Not a string
                    Dim ToStringFunction As FunctionNode = Nothing
                    If Not DirectCast(node, ValueNode).ReturnType = STD_str Then
                        For Each fn As FunctionNode In DirectCast(node, ValueNode).ReturnType.Methods
                            If fn.FunctionName = "str" Then
                                ToStringFunction = fn
                                'ThrowNodeTypeException("SNC02", "A value of type ""str"" was expected instead of type """ & DirectCast(node, ValueNode).ReturnType.ToString() & """.", node, "Convert your value using the .str() method.")
                                Exit For
                            End If
                        Next
                        If ToStringFunction Is Nothing Then
                            ThrowNodeTypeException("SNC02", "A value of type ""str"" was expected instead of type """ & DirectCast(node, ValueNode).ReturnType.ToString() & """.", node)
                        End If
                    End If

                    'Compile
                    ResultValue &= "%s"
                    Dim FVariableName As String = GetVariableCompiledName()
                    If ToStringFunction IsNot Nothing Then
                        ToStringFunction.Compile(Nothing)
                        content.Add("char * " & FVariableName & " = " & ToStringFunction.CompiledName & "(GV, " & DirectCast(node, ValueNode).Compile(content) & ");")
                    Else
                        content.Add("char * " & FVariableName & " = " & DirectCast(node, ValueNode).Compile(content) & ";")
                    End If
                    Fvariables.Add(FVariableName)

                    Gettings = False
                    LastGet = ""

                End If

            Else

                LastGet &= WorkingString(0)
                WorkingString = WorkingString.Substring(1)

                If LastGet.EndsWith("{") Then
                    ResultValue &= LastGet.Substring(0, LastGet.Length - 1)
                    LastGet = ""
                    Gettings = True
                End If

            End If

        End While
        ResultValue &= LastGet

        Dim FVariablesList As String = ""
        Dim FVariablesLen As String = ""
        Dim StringTotalLen As String = GetVariableCompiledName()
        Dim StringName As String = GetVariableCompiledName()
        content.Add("char * " & StringName & " = new_str(""" & ResultValue & """);")
        For Each var As String In Fvariables
            FVariablesList &= ", " & var
            FVariablesLen &= " + strlen(" & var & ")"
        Next
        If FVariablesList.StartsWith(", ") Then
            FVariablesList = FVariablesList.Substring(2)
        End If
        If FVariablesLen.StartsWith(" + ") Then
            content.Add("int " & StringTotalLen & " = strlen(" & StringName & ")" & FVariablesLen & ";")
        Else
            content.Add("int " & StringTotalLen & " = strlen(" & StringName & ");")
        End If
        content.Add("tgc_free(&gc, " & StringName & ");")
        content.Add(StringName & " = tgc_alloc_opt(&gc, sizeof(char) * " & StringTotalLen & ", TGC_LEAF, NULL);")
        If Fvariables.Count > 0 Then
            content.Add("sprintf(" & StringName & ", """ & ResultValue & """, " & FVariablesList & ");")
        Else
            content.Add("sprintf(" & StringName & ", """ & ResultValue & """);")
        End If
        Return StringName


    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return True
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type
        Return STD_str
    End Function

End Class
