Module Helper

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly VBTemplatesFolder As String = (System.Reflection.Assembly.GetExecutingAssembly().Location().Substring(0, System.Reflection.Assembly.GetExecutingAssembly().Location().LastIndexOf("\")) & "\templates").Replace("\", "/")
    Public ReadOnly AppData As String = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData).Replace("\", "/")

End Module
