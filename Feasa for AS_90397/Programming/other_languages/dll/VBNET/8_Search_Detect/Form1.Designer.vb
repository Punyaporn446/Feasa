<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form reemplaza a Dispose para limpiar la lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requerido por el Diseñador de Windows Forms
    Private components As System.ComponentModel.IContainer

    'NOTA: el Diseñador de Windows Forms necesita el siguiente procedimiento
    'Se puede modificar usando el Diseñador de Windows Forms.  
    'No lo modifique con el editor de código.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnDetectSerials = New System.Windows.Forms.Button()
        Me.btnDetectPorts = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.cmdSearch = New System.Windows.Forms.Button()
        Me.txtSN = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.txtLog = New System.Windows.Forms.TextBox()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.btnDetectSerials)
        Me.GroupBox1.Controls.Add(Me.btnDetectPorts)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(173, 342)
        Me.GroupBox1.TabIndex = 3
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Info"
        '
        'btnDetectSerials
        '
        Me.btnDetectSerials.Location = New System.Drawing.Point(6, 293)
        Me.btnDetectSerials.Name = "btnDetectSerials"
        Me.btnDetectSerials.Size = New System.Drawing.Size(160, 43)
        Me.btnDetectSerials.TabIndex = 19
        Me.btnDetectSerials.Text = "Detect Serials"
        Me.btnDetectSerials.UseVisualStyleBackColor = True
        '
        'btnDetectPorts
        '
        Me.btnDetectPorts.Location = New System.Drawing.Point(6, 244)
        Me.btnDetectPorts.Name = "btnDetectPorts"
        Me.btnDetectPorts.Size = New System.Drawing.Size(160, 43)
        Me.btnDetectPorts.TabIndex = 18
        Me.btnDetectPorts.Text = "Detect Ports"
        Me.btnDetectPorts.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(6, 16)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(161, 142)
        Me.Label3.TabIndex = 17
        Me.Label3.Text = resources.GetString("Label3.Text")
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.cmdSearch)
        Me.GroupBox2.Controls.Add(Me.txtSN)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Location = New System.Drawing.Point(195, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(180, 106)
        Me.GroupBox2.TabIndex = 4
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Search"
        '
        'cmdSearch
        '
        Me.cmdSearch.Location = New System.Drawing.Point(9, 47)
        Me.cmdSearch.Name = "cmdSearch"
        Me.cmdSearch.Size = New System.Drawing.Size(160, 43)
        Me.cmdSearch.TabIndex = 14
        Me.cmdSearch.Text = "FIND LED ANALYSER && SHOW THE STATUS"
        Me.cmdSearch.UseVisualStyleBackColor = True
        '
        'txtSN
        '
        Me.txtSN.Location = New System.Drawing.Point(125, 21)
        Me.txtSN.MaxLength = 4
        Me.txtSN.Name = "txtSN"
        Me.txtSN.Size = New System.Drawing.Size(44, 20)
        Me.txtSN.TabIndex = 13
        Me.txtSN.Text = "E001"
        '
        'Label4
        '
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(6, 21)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(118, 20)
        Me.Label4.TabIndex = 5
        Me.Label4.Text = "Serial Number to Find:"
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.txtLog)
        Me.GroupBox3.Location = New System.Drawing.Point(195, 124)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(180, 230)
        Me.GroupBox3.TabIndex = 5
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Status"
        '
        'txtLog
        '
        Me.txtLog.Location = New System.Drawing.Point(6, 19)
        Me.txtLog.Multiline = True
        Me.txtLog.Name = "txtLog"
        Me.txtLog.Size = New System.Drawing.Size(168, 205)
        Me.txtLog.TabIndex = 8
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(387, 366)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Search Led Analyser - (c) Feasa Enterprises Ltd"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents txtLog As System.Windows.Forms.TextBox
    Friend WithEvents txtSN As System.Windows.Forms.TextBox
    Friend WithEvents cmdSearch As System.Windows.Forms.Button
    Friend WithEvents btnDetectSerials As Button
    Friend WithEvents btnDetectPorts As Button
    Friend WithEvents Label3 As Label
End Class
