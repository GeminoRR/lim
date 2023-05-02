'========================================
'========== NUMERIC VALUE NODE ==========
'========================================
'
' Represents a call to a property "b" of an object "a"
' a.b
'
Class ChildNode
    Inherits ValueNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Obj As ValueNode
    Public PropertieName As String

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer, ByVal Obj As ValueNode, ByVal PropertieName As String)

        'Inherits
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)

        'Properties
        Me.Obj = Obj
        Me.Obj.ParentNode = Me
        Me.PropertieName = PropertieName

    End Sub

    '===============================
    '========== TO STRING ==========
    '===============================
    Public Overrides Function ToString() As String
        Return "(" & Obj.ToString() & ")." & PropertieName
    End Function

    '=============================
    '========== COMPILE ==========
    '=============================
    Public Overrides Function Compile(content As List(Of String)) As String

        'Search propertie
        For Each var As Variable In Obj.ReturnType.Variables
            If var.VariableName = Me.PropertieName Then
                Return "(" & Obj.Compile(content) & ")->" & var.CompiledName
            End If
        Next

        'Search function
        For Each fun As FunctionNode In Obj.ReturnType.Methods
            If fun.FunctionName = Me.PropertieName Then

                'Error
                If Me.PropertieName = "new" Then
                    ThrowNodeSyntaxException("CNC02", "The constructor of a class cannot be called independently from the ""new"" statement.", Me)
                End If

                'Create temp variable
                Dim TempVar As String = GetVariableCompiledName()

                'Compile the variable
                content.Add(fun.MinimumFunctionType.CompiledName & " * " & TempVar & " = " & fun.MinimumFunctionType.CompiledName & "_allocate();")
                content.Add(TempVar & "->object = " & Obj.Compile(content) & ";")
                content.Add(TempVar & "->target = " & fun.MinimumFunctionCompiledName & ";")

                'Return
                Return TempVar

            End If
        Next

        'Error
        ThrowNodeTypeException("CNC01", "Unable to find the property or method named """ & Me.PropertieName & """ of the object of type """ & Obj.ReturnType.ToString() & """.", Me)
        Return Nothing

    End Function

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected Overrides Function CheckIsConstant() As Boolean
        Return False
    End Function

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected Overrides Function NodeReturnType() As Type

        'Search propertie
        For Each var As Variable In Obj.ReturnType.Variables
            If var.VariableName = Me.PropertieName Then
                Return var.ValueType
            End If
        Next

        'Search function
        For Each fun As FunctionNode In Obj.ReturnType.Methods
            If fun.FunctionName = Me.PropertieName Then
                If Me.PropertieName = "new" Then
                    ThrowNodeSyntaxException("CNNRT02", "The constructor of a class cannot be called independently from the ""new"" statement.", Me)
                End If
                Return fun.MinimumFunctionType
            End If
        Next

        'Error
        ThrowNodeTypeException("CNNRT01", "Unable to find the property or method named """ & Me.PropertieName & """ of the object of type """ & Obj.ReturnType.ToString() & """.", Me)
        Return Nothing

    End Function

    '=======================================
    '========== TARGETED FUNCTION ==========
    '=======================================
    Public Function TargetedFunction() As FunctionNode

        'Search propertie
        For Each var As Variable In Obj.ReturnType.Variables
            If var.VariableName = Me.PropertieName Then
                Return Nothing
            End If
        Next

        'Search function
        For Each fun As FunctionNode In Obj.ReturnType.Methods
            If fun.FunctionName = Me.PropertieName Then
                If Me.PropertieName = "new" Then
                    ThrowNodeSyntaxException("CNTF01", "The constructor of a class cannot be called independently from the ""new"" statement.", Me)
                End If
                Return fun
            End If
        Next

        'Return 
        Return Nothing

    End Function

End Class
