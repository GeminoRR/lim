'==========================
'========== NODE ==========
'==========================
'
' Node class.
'
MustInherit Class Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public PositionStartY As Integer
    Public PositionStartX As Integer
    Public PositionEndY As Integer
    Public PositionEndX As Integer
    Public ParentNode As Node

    '=================================
    '========== PARENT FILE ==========
    '=================================
    Private _ParentFile As SourceFile
    <System.Diagnostics.DebuggerBrowsable(DebuggerBrowsableState.Never)>
    Public ReadOnly Property ParentFile As SourceFile
        Get
            If Me._ParentFile Is Nothing Then
                Dim Temp As Node = Me
                While Temp.ParentNode IsNot Nothing
                    Temp = Temp.ParentNode
                End While
                If Not TypeOf Temp Is SourceFile Then
                    ThrowSimpleLimException("SNNPF01", "Internal problem", "Unable to find the source file linked to the current node.")
                End If
                Me._ParentFile = Temp
            End If
            Return Me._ParentFile
        End Get
    End Property

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)
        Me.ParentNode = Nothing
        Me.PositionStartY = PositionStartY
        Me.PositionStartX = PositionStartX
        Me.PositionEndY = PositionEndY
        Me.PositionEndX = PositionEndX
        Me._ParentFile = Nothing
    End Sub

    '==============================
    '========== TOSTRING ==========
    '==============================
    Public Overrides Function ToString() As String
        Return "()"
    End Function

    '===========================
    '========== CLONE ==========
    '===========================
    Public Function Clone(Optional ParentNode As Node = Nothing) As Node
        If ParentNode Is Nothing Then
            Return Duplicate()
        Else
            Dim Cloned As Node = Duplicate()
            Cloned.ParentNode = ParentNode
            Return Cloned
        End If
    End Function
    Protected MustOverride Function Duplicate() As Node

    '==================================
    '========== PARENT SCOPE ==========
    '==================================
    Public ReadOnly Property ParentScope As ScopeNode
        Get
            Dim Parent As Node = Me
            While Parent.ParentNode IsNot Nothing

                Parent = Parent.ParentNode

                If TypeOf Parent Is ScopeNode Then
                    Return DirectCast(Parent, ScopeNode)
                End If

            End While
            Return Nothing
        End Get
    End Property

End Class



'===============================
'========== STATEMENT ==========
'================================
'
' Represents a line (declare variable / if / function / class).
'
MustInherit Class StatementNode
    Inherits Node

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)
    End Sub

    '=============================
    '========== COMPILE ==========
    '=============================
    Public MustOverride Sub Compile(ByVal content As List(Of String))

End Class


'===========================
'========== SCOPE ==========
'===========================
'
' Represents a node containing variables.
'
MustInherit Class ScopeNode
    Inherits StatementNode

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public Variables As New List(Of Variable)

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)
    End Sub

End Class

'===========================
'========== VALUE ==========
'===========================
'
' Represents a node containing a value.
'
MustInherit Class ValueNode
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly Property IsConstant As Boolean
        Get
            If Me._Constant = Nothing Then
                Me._Constant = Me.CheckIsConstant()
            End If
            Return Me._Constant
        End Get
    End Property
    Private _Constant As Boolean

    Public ReadOnly Property ReturnType As Lim.Type
        Get
            If Me._ReturnType Is Nothing Then
                Me._ReturnType = Me.NodeReturnType()
            End If
            Return Me._ReturnType
        End Get
    End Property
    Private _ReturnType As Lim.Type


    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal PositionStartY As Integer, ByVal PositionStartX As Integer, ByVal PositionEndY As Integer, ByVal PositionEndX As Integer)
        MyBase.New(PositionStartY, PositionStartX, PositionEndY, PositionEndX)
        Me._Constant = Nothing
    End Sub

    '=================================
    '========== IS CONSTANT ==========
    '=================================
    Protected MustOverride Function CheckIsConstant() As Boolean

    '=================================
    '========== RETURN TYPE ==========
    '=================================
    Protected MustOverride Function NodeReturnType() As Type

    '=============================
    '========== COMPILE ==========
    '=============================
    Public MustOverride Function Compile(ByVal content As List(Of String)) As String

End Class