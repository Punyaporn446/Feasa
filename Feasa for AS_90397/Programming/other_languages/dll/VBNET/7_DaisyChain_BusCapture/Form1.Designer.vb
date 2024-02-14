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
        Me.Label1 = New System.Windows.Forms.Label
        Me.lstPorts = New System.Windows.Forms.ComboBox
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.Label3 = New System.Windows.Forms.Label
        Me.GroupBox3 = New System.Windows.Forms.GroupBox
        Me.txtLog = New System.Windows.Forms.TextBox
        Me.GroupBox4 = New System.Windows.Forms.GroupBox
        Me.lstCaptureRange = New System.Windows.Forms.ComboBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.lstCaptureMode = New System.Windows.Forms.ComboBox
        Me.Label4 = New System.Windows.Forms.Label
        Me.txtFiber = New System.Windows.Forms.TextBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.cmdRead = New System.Windows.Forms.Button
        Me.lstAnalysers = New System.Windows.Forms.ListBox
        Me.cmdAnalysers = New System.Windows.Forms.Button
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(6, 21)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(167, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "COM Port of the 1st Led Analyser:"
        '
        'lstPorts
        '
        Me.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstPorts.FormattingEnabled = True
        Me.lstPorts.Location = New System.Drawing.Point(109, 37)
        Me.lstPorts.Name = "lstPorts"
        Me.lstPorts.Size = New System.Drawing.Size(64, 21)
        Me.lstPorts.TabIndex = 2
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(183, 192)
        Me.GroupBox1.TabIndex = 3
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications Setup"
        '
        'Label3
        '
        Me.Label3.ForeColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Label3.Location = New System.Drawing.Point(6, 81)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(171, 85)
        Me.Label3.TabIndex = 5
        Me.Label3.Text = "Select the port of the 1st Analyser (Bus Master) and then add all the devices att" & _
            "ached to the Bus in the list below"
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.txtLog)
        Me.GroupBox3.Location = New System.Drawing.Point(201, 210)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(224, 120)
        Me.GroupBox3.TabIndex = 5
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Response"
        '
        'txtLog
        '
        Me.txtLog.Location = New System.Drawing.Point(9, 19)
        Me.txtLog.Multiline = True
        Me.txtLog.Name = "txtLog"
        Me.txtLog.Size = New System.Drawing.Size(207, 90)
        Me.txtLog.TabIndex = 8
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.lstCaptureRange)
        Me.GroupBox4.Controls.Add(Me.Label2)
        Me.GroupBox4.Controls.Add(Me.lstCaptureMode)
        Me.GroupBox4.Controls.Add(Me.Label4)
        Me.GroupBox4.Controls.Add(Me.txtFiber)
        Me.GroupBox4.Controls.Add(Me.Label6)
        Me.GroupBox4.Controls.Add(Me.cmdRead)
        Me.GroupBox4.Location = New System.Drawing.Point(202, 12)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(223, 192)
        Me.GroupBox4.TabIndex = 6
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Measure"
        '
        'lstCaptureRange
        '
        Me.lstCaptureRange.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstCaptureRange.FormattingEnabled = True
        Me.lstCaptureRange.Location = New System.Drawing.Point(123, 64)
        Me.lstCaptureRange.Name = "lstCaptureRange"
        Me.lstCaptureRange.Size = New System.Drawing.Size(92, 21)
        Me.lstCaptureRange.TabIndex = 25
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(4, 67)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(85, 13)
        Me.Label2.TabIndex = 24
        Me.Label2.Text = "Range (manual):"
        '
        'lstCaptureMode
        '
        Me.lstCaptureMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstCaptureMode.FormattingEnabled = True
        Me.lstCaptureMode.Location = New System.Drawing.Point(8, 37)
        Me.lstCaptureMode.Name = "lstCaptureMode"
        Me.lstCaptureMode.Size = New System.Drawing.Size(207, 21)
        Me.lstCaptureMode.TabIndex = 23
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(5, 21)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(37, 13)
        Me.Label4.TabIndex = 22
        Me.Label4.Text = "Mode:"
        '
        'txtFiber
        '
        Me.txtFiber.Location = New System.Drawing.Point(83, 109)
        Me.txtFiber.MaxLength = 2
        Me.txtFiber.Name = "txtFiber"
        Me.txtFiber.Size = New System.Drawing.Size(30, 20)
        Me.txtFiber.TabIndex = 12
        Me.txtFiber.Text = "1"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(3, 112)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(74, 13)
        Me.Label6.TabIndex = 11
        Me.Label6.Text = "Fiber to Read:"
        '
        'cmdRead
        '
        Me.cmdRead.Location = New System.Drawing.Point(6, 139)
        Me.cmdRead.Name = "cmdRead"
        Me.cmdRead.Size = New System.Drawing.Size(209, 27)
        Me.cmdRead.TabIndex = 10
        Me.cmdRead.Text = "BUS CAPTURE + READ"
        Me.cmdRead.UseVisualStyleBackColor = True
        '
        'lstAnalysers
        '
        Me.lstAnalysers.FormattingEnabled = True
        Me.lstAnalysers.Location = New System.Drawing.Point(6, 53)
        Me.lstAnalysers.Name = "lstAnalysers"
        Me.lstAnalysers.Size = New System.Drawing.Size(167, 56)
        Me.lstAnalysers.TabIndex = 7
        '
        'cmdAnalysers
        '
        Me.cmdAnalysers.Location = New System.Drawing.Point(9, 19)
        Me.cmdAnalysers.Name = "cmdAnalysers"
        Me.cmdAnalysers.Size = New System.Drawing.Size(164, 28)
        Me.cmdAnalysers.TabIndex = 7
        Me.cmdAnalysers.Text = "Add Analyser:"
        Me.cmdAnalysers.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.cmdAnalysers)
        Me.GroupBox2.Controls.Add(Me.lstAnalysers)
        Me.GroupBox2.Location = New System.Drawing.Point(12, 210)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(183, 120)
        Me.GroupBox2.TabIndex = 8
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Daisy Chain"
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(444, 343)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Daisy Chain Bus Capture - (c) Feasa Enterprises Ltd"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents lstPorts As System.Windows.Forms.ComboBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents txtLog As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Friend WithEvents txtFiber As System.Windows.Forms.TextBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents cmdRead As System.Windows.Forms.Button
    Friend WithEvents cmdAnalysers As System.Windows.Forms.Button
    Friend WithEvents lstAnalysers As System.Windows.Forms.ListBox
    Friend WithEvents lstCaptureRange As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents lstCaptureMode As System.Windows.Forms.ComboBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox

End Class
