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
        Me.Label1 = New System.Windows.Forms.Label
        Me.cmdRead = New System.Windows.Forms.Button
        Me.lstPorts = New System.Windows.Forms.ComboBox
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.lstBaudrate = New System.Windows.Forms.ComboBox
        Me.optBaudrateManual = New System.Windows.Forms.RadioButton
        Me.optBaudrateAuto = New System.Windows.Forms.RadioButton
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.txtFiber = New System.Windows.Forms.TextBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.txtLog = New System.Windows.Forms.TextBox
        Me.Label5 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(6, 21)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(56, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "COM Port:"
        '
        'cmdRead
        '
        Me.cmdRead.Location = New System.Drawing.Point(9, 111)
        Me.cmdRead.Name = "cmdRead"
        Me.cmdRead.Size = New System.Drawing.Size(159, 27)
        Me.cmdRead.TabIndex = 1
        Me.cmdRead.Text = "CAPTURE AND READ"
        Me.cmdRead.UseVisualStyleBackColor = True
        '
        'lstPorts
        '
        Me.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstPorts.FormattingEnabled = True
        Me.lstPorts.Location = New System.Drawing.Point(68, 18)
        Me.lstPorts.Name = "lstPorts"
        Me.lstPorts.Size = New System.Drawing.Size(64, 21)
        Me.lstPorts.TabIndex = 2
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.lstBaudrate)
        Me.GroupBox1.Controls.Add(Me.optBaudrateManual)
        Me.GroupBox1.Controls.Add(Me.optBaudrateAuto)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(183, 295)
        Me.GroupBox1.TabIndex = 3
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications Setup"
        '
        'lstBaudrate
        '
        Me.lstBaudrate.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstBaudrate.Enabled = False
        Me.lstBaudrate.FormattingEnabled = True
        Me.lstBaudrate.Location = New System.Drawing.Point(82, 98)
        Me.lstBaudrate.Name = "lstBaudrate"
        Me.lstBaudrate.Size = New System.Drawing.Size(74, 21)
        Me.lstBaudrate.TabIndex = 10
        '
        'optBaudrateManual
        '
        Me.optBaudrateManual.AutoSize = True
        Me.optBaudrateManual.Location = New System.Drawing.Point(20, 99)
        Me.optBaudrateManual.Name = "optBaudrateManual"
        Me.optBaudrateManual.Size = New System.Drawing.Size(66, 17)
        Me.optBaudrateManual.TabIndex = 9
        Me.optBaudrateManual.Text = "Manual: "
        Me.optBaudrateManual.UseVisualStyleBackColor = True
        '
        'optBaudrateAuto
        '
        Me.optBaudrateAuto.AutoSize = True
        Me.optBaudrateAuto.Checked = True
        Me.optBaudrateAuto.Location = New System.Drawing.Point(20, 79)
        Me.optBaudrateAuto.Name = "optBaudrateAuto"
        Me.optBaudrateAuto.Size = New System.Drawing.Size(47, 17)
        Me.optBaudrateAuto.TabIndex = 8
        Me.optBaudrateAuto.TabStop = True
        Me.optBaudrateAuto.Text = "Auto"
        Me.optBaudrateAuto.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(6, 63)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(53, 13)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "Baudrate:"
        '
        'Label2
        '
        Me.Label2.Location = New System.Drawing.Point(6, 168)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(171, 116)
        Me.Label2.TabIndex = 6
        Me.Label2.Text = resources.GetString("Label2.Text")
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.txtFiber)
        Me.GroupBox2.Controls.Add(Me.Label6)
        Me.GroupBox2.Controls.Add(Me.txtLog)
        Me.GroupBox2.Controls.Add(Me.Label5)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Controls.Add(Me.cmdRead)
        Me.GroupBox2.Location = New System.Drawing.Point(201, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(174, 295)
        Me.GroupBox2.TabIndex = 4
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Commands"
        '
        'txtFiber
        '
        Me.txtFiber.Location = New System.Drawing.Point(86, 81)
        Me.txtFiber.MaxLength = 2
        Me.txtFiber.Name = "txtFiber"
        Me.txtFiber.Size = New System.Drawing.Size(30, 20)
        Me.txtFiber.TabIndex = 9
        Me.txtFiber.Text = "1"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(6, 84)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(74, 13)
        Me.Label6.TabIndex = 8
        Me.Label6.Text = "Fiber to Read:"
        '
        'txtLog
        '
        Me.txtLog.Location = New System.Drawing.Point(9, 178)
        Me.txtLog.Multiline = True
        Me.txtLog.Name = "txtLog"
        Me.txtLog.Size = New System.Drawing.Size(154, 106)
        Me.txtLog.TabIndex = 7
        '
        'Label5
        '
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(6, 153)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(162, 22)
        Me.Label5.TabIndex = 6
        Me.Label5.Text = "Response:"
        '
        'Label4
        '
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(6, 21)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(162, 53)
        Me.Label4.TabIndex = 5
        Me.Label4.Text = "Press the button shown below to execute a capture and read the result."
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(387, 319)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Baudrate - (c) Feasa Enterprises Ltd"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents cmdRead As System.Windows.Forms.Button
    Friend WithEvents lstPorts As System.Windows.Forms.ComboBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents txtLog As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents txtFiber As System.Windows.Forms.TextBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents lstBaudrate As System.Windows.Forms.ComboBox
    Friend WithEvents optBaudrateManual As System.Windows.Forms.RadioButton
    Friend WithEvents optBaudrateAuto As System.Windows.Forms.RadioButton
    Friend WithEvents Label3 As System.Windows.Forms.Label

End Class
