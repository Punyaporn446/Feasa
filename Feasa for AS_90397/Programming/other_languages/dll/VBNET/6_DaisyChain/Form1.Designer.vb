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
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.cmdFree = New System.Windows.Forms.Button
        Me.cmdGet = New System.Windows.Forms.Button
        Me.txtSN = New System.Windows.Forms.TextBox
        Me.Label4 = New System.Windows.Forms.Label
        Me.GroupBox3 = New System.Windows.Forms.GroupBox
        Me.txtLog = New System.Windows.Forms.TextBox
        Me.GroupBox4 = New System.Windows.Forms.GroupBox
        Me.txtFiber = New System.Windows.Forms.TextBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.cmdRead = New System.Windows.Forms.Button
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
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
        Me.GroupBox1.Size = New System.Drawing.Size(183, 342)
        Me.GroupBox1.TabIndex = 3
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications Setup"
        '
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(6, 81)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(171, 85)
        Me.Label3.TabIndex = 5
        Me.Label3.Text = "First, you need to select the communications port where the 1st Led Analyser of t" & _
            "he BUS is connected. This will be the port through which you can access to the b" & _
            "us"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.cmdFree)
        Me.GroupBox2.Controls.Add(Me.cmdGet)
        Me.GroupBox2.Controls.Add(Me.txtSN)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Location = New System.Drawing.Point(201, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(174, 128)
        Me.GroupBox2.TabIndex = 4
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Daisy Chain"
        '
        'cmdFree
        '
        Me.cmdFree.Location = New System.Drawing.Point(93, 88)
        Me.cmdFree.Name = "cmdFree"
        Me.cmdFree.Size = New System.Drawing.Size(70, 25)
        Me.cmdFree.TabIndex = 15
        Me.cmdFree.Text = "FREE BUS"
        Me.cmdFree.UseVisualStyleBackColor = True
        '
        'cmdGet
        '
        Me.cmdGet.Location = New System.Drawing.Point(9, 88)
        Me.cmdGet.Name = "cmdGet"
        Me.cmdGet.Size = New System.Drawing.Size(70, 25)
        Me.cmdGet.TabIndex = 14
        Me.cmdGet.Text = "GET BUS"
        Me.cmdGet.UseVisualStyleBackColor = True
        '
        'txtSN
        '
        Me.txtSN.Location = New System.Drawing.Point(9, 61)
        Me.txtSN.MaxLength = 4
        Me.txtSN.Name = "txtSN"
        Me.txtSN.Size = New System.Drawing.Size(44, 20)
        Me.txtSN.TabIndex = 13
        Me.txtSN.Text = "E001"
        '
        'Label4
        '
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(6, 21)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(162, 37)
        Me.Label4.TabIndex = 5
        Me.Label4.Text = "Serial Number of the Led Analyser you want to control:"
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.txtLog)
        Me.GroupBox3.Location = New System.Drawing.Point(201, 253)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(173, 101)
        Me.GroupBox3.TabIndex = 5
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Response"
        '
        'txtLog
        '
        Me.txtLog.Location = New System.Drawing.Point(9, 19)
        Me.txtLog.Multiline = True
        Me.txtLog.Name = "txtLog"
        Me.txtLog.Size = New System.Drawing.Size(154, 73)
        Me.txtLog.TabIndex = 8
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.txtFiber)
        Me.GroupBox4.Controls.Add(Me.Label6)
        Me.GroupBox4.Controls.Add(Me.cmdRead)
        Me.GroupBox4.Location = New System.Drawing.Point(202, 146)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(173, 101)
        Me.GroupBox4.TabIndex = 6
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Measure"
        '
        'txtFiber
        '
        Me.txtFiber.Location = New System.Drawing.Point(85, 22)
        Me.txtFiber.MaxLength = 2
        Me.txtFiber.Name = "txtFiber"
        Me.txtFiber.Size = New System.Drawing.Size(30, 20)
        Me.txtFiber.TabIndex = 12
        Me.txtFiber.Text = "1"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(5, 25)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(74, 13)
        Me.Label6.TabIndex = 11
        Me.Label6.Text = "Fiber to Read:"
        '
        'cmdRead
        '
        Me.cmdRead.Location = New System.Drawing.Point(8, 52)
        Me.cmdRead.Name = "cmdRead"
        Me.cmdRead.Size = New System.Drawing.Size(159, 27)
        Me.cmdRead.TabIndex = 10
        Me.cmdRead.Text = "CAPTURE AND READ"
        Me.cmdRead.UseVisualStyleBackColor = True
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(387, 366)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Daisy Chain - (c) Feasa Enterprises Ltd"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents lstPorts As System.Windows.Forms.ComboBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents txtLog As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Friend WithEvents cmdFree As System.Windows.Forms.Button
    Friend WithEvents cmdGet As System.Windows.Forms.Button
    Friend WithEvents txtSN As System.Windows.Forms.TextBox
    Friend WithEvents txtFiber As System.Windows.Forms.TextBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents cmdRead As System.Windows.Forms.Button

End Class
