'==============================
'========= ReturnNode =========
'==============================
Public Class ReturnNode
    Inherits node

    'Variables
    Public value As node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal node As node)

        MyBase.New(positionStart, positionEnd)
        Me.value = node
        Me.value.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("return {0}", value.ToString())
    End Function

End Class

'========================================
'========= BracketsSelectorNode =========
'========================================
Public Class BracketsSelectorNode
    Inherits node

    'Variable
    Public Target As Node
    Public index As Node

    Public target_relation As RelationNode

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Target As node, ByVal index As node)

        MyBase.New(positionStart, positionEnd)
        Me.Target = Target
        Me.Target.parentNode = Me
        Me.index = index
        Me.index.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0}[{1}])", Target.ToString(), index.ToString())
    End Function

End Class

'============================
'========= ListNode =========
'============================
Public Class ListNode
    Inherits node

    'Variable
    Public elements As New List(Of node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer)

        MyBase.New(positionStart, positionEnd)

    End Sub

    'Add element
    Public Sub addElement(ByVal elm As node)
        elm.parentNode = Me
        elements.Add(elm)
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim elementToString As String = ""
        For Each elm As node In elements
            elementToString &= ", " & elm.ToString()
        Next
        If elementToString.StartsWith(", ") Then
            elementToString = elementToString.Substring(2)
        End If
        Return "[" & elementToString & "]"
    End Function

End Class

'===========================
'========= MapNode =========
'===========================
Public Class MapNode
    Inherits node

    'Variable
    Public elements As New List(Of List(Of node))

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer)

        MyBase.New(positionStart, positionEnd)

    End Sub

    'Add element
    Public Sub addElement(ByVal key As node, ByVal value As node)
        key.parentNode = Me
        value.parentNode = Me
        elements.Add({key, value}.ToList())
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim elementToString As String = ""
        For Each elm As List(Of node) In elements
            elementToString &= ", " & elm(0).ToString() & ":" & elm(1).ToString()
        Next
        If elementToString.StartsWith(", ") Then
            elementToString = elementToString.Substring(2)
        End If
        Return "{" & elementToString & "}"
    End Function

End Class

'====================================
'========= FunctionCallNode =========
'====================================
Public Class FunctionCallNode
    Inherits Node

    'Variable
    Public FunctionName As String
    Public Arguments As New List(Of Node)

    Public allLineFunction As Boolean

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal FunctionName As String, ByVal Arguments As List(Of Node))

        MyBase.New(positionStart, positionEnd)
        Me.allLineFunction = False
        Me.FunctionName = FunctionName
        For Each Arg As Node In Arguments
            Arg.parentNode = Me
            Me.Arguments.Add(Arg)
        Next

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim ATS As String = ""
        For Each arg As Node In Arguments
            ATS &= ", " & arg.ToString()
        Next
        If ATS.StartsWith(", ") Then
            ATS = ATS.Substring(2)
        End If
        Return FunctionName.ToString & "(" & ATS & ")"
    End Function

End Class

'================================
'========= VariableNode =========
'================================
Public Class VariableNode
    Inherits Node

    'Variable
    Public VariableName As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal VariableName As String)

        MyBase.New(positionStart, positionEnd)
        Me.VariableName = VariableName

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return VariableName.ToString()
    End Function

End Class

'==================================
'========= ComparisonNode =========
'==================================
Public Class ComparisonNode
    Inherits node

    'Variable
    Public leftNode As node
    Public op As token
    Public rightNode As Node

    Public target_relation As RelationNode = Nothing

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal leftNode As node, ByVal op As token, ByVal rightNode As node)

        MyBase.New(positionStart, positionEnd)
        Me.leftNode = leftNode
        Me.leftNode.parentNode = Me
        Me.op = op
        Me.rightNode = rightNode
        Me.rightNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1} {2})", leftNode, op, rightNode)
    End Function

End Class

'===============================
'========= BooleanNode =========
'===============================
Public Class BooleanNode
    Inherits node

    'Variable
    Public value As Boolean

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As Boolean)

        MyBase.New(positionStart, positionEnd)
        Me.value = value

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return value.ToString()
    End Function

End Class

'==============================
'========= StringNode =========
'==============================
Public Class StringNode
    Inherits node

    'Variables
    Public value As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As String)

        MyBase.New(positionStart, positionEnd)
        Me.value = value

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return """" & value & """"
    End Function

End Class

'================================
'========= F-StringNode =========
'================================
Public Class FStringNode
    Inherits Node

    'Variables
    Public value As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As String)

        MyBase.New(positionStart, positionEnd)
        Me.value = value

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "f""" & value & """"
    End Function

End Class

'=================================
'========= AddSourceNode =========
'=================================
Public Class AddSourceNode
    Inherits Node

    'Variables
    Public value As String
    Public allLine As Boolean

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As String)

        MyBase.New(positionStart, positionEnd)
        Me.value = value
        Me.allLine = False

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "$""" & value & """"
    End Function

End Class

'=============================
'========= ValueNode =========
'=============================
Public Class valueNode
    Inherits node

    'Variables
    Public tok As token

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal tok As token)

        MyBase.New(positionStart, positionEnd)
        Me.tok = tok

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Select Case tok.type
            Case tokenType.CT_FLOAT
                If tok.value.Contains(".") Then
                    Return tok.value
                Else
                    Return tok.value & ".0"
                End If
            Case tokenType.CT_INTEGER
                Return tok.value
            Case Else
                Return "" & tok.ToString() & ""
        End Select
    End Function

End Class

'===============================
'========= UnaryOpNode =========
'===============================
Public Class UnaryOpNode
    Inherits node

    'Variables
    Public op As token
    Public node As Node

    Public target_relation As RelationNode = Nothing

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal op As token, ByVal node As node)

        MyBase.New(positionStart, positionEnd)
        Me.node = node
        Me.node.parentNode = Me
        Me.op = op

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1})", op, node)
    End Function

End Class


'=============================
'========= BinOpNode =========
'=============================
Public Class binOpNode
    Inherits node

    'Variables
    Public leftNode As node
    Public op As token
    Public rightNode As Node

    Public target_relation As RelationNode = Nothing

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal leftNode As node, ByVal op As token, ByVal rightNode As node)

        MyBase.New(positionStart, positionEnd)
        Me.leftNode = leftNode
        Me.leftNode.parentNode = Me
        Me.op = op
        Me.rightNode = rightNode
        Me.rightNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1} {2})", leftNode, op, rightNode)
    End Function

End Class

'==============================
'========= BoolOpNode =========
'==============================
Public Class boolOpNode
    Inherits Node

    'Variables
    Public leftNode As Node
    Public op As token
    Public rightNode As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal leftNode As Node, ByVal op As token, ByVal rightNode As Node)

        MyBase.New(positionStart, positionEnd)
        Me.leftNode = leftNode
        Me.leftNode.parentNode = Me
        Me.op = op
        Me.rightNode = rightNode
        Me.rightNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1} {2})", leftNode, op, rightNode)
    End Function

End Class

'==============================
'========= Child Node =========
'==============================
Public Class childNode
    Inherits node

    'Variables
    Public parentStruct As Node
    Public childNode As Node
    Public allLine As Boolean

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal left As node, ByVal right As node)

        MyBase.New(positionStart, positionEnd)

        Me.parentStruct = left
        Me.parentStruct.parentNode = Me

        Me.childNode = right
        Me.childNode.parentNode = Me

        Me.allLine = False

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0}.{1})", parentStruct.ToString(), childNode.ToString())
    End Function

End Class

'============================
'========= New Node =========
'============================
Public Class newNode
    Inherits Node

    'Variables
    Public type As typeNode
    Public arguments As List(Of Node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal type As typeNode, ByVal arguments As List(Of Node))

        MyBase.New(positionStart, positionEnd)
        Me.type = type
        Me.arguments = arguments
        For Each arg As Node In Me.arguments
            arg.parentNode = Me
        Next

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim argSTR As String = ""
        For Each arg As Node In arguments
            argSTR &= ", " & arg.ToString()
        Next
        If argSTR.StartsWith(", ") Then
            argSTR = argSTR.Substring(2)
        End If
        Return String.Format("(New {0}({1}))", type.ToString(), argSTR)
    End Function

End Class