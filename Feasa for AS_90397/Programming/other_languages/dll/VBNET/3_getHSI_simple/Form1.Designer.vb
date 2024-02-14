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
        Me.cmdRead = New System.Windows.Forms.Button
        Me.lstPorts = New System.Windows.Forms.ComboBox
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.lblIntensity = New System.Windows.Forms.Label
        Me.lblSat = New System.Windows.Forms.Label
        Me.lblHue = New System.Windows.Forms.Label
        Me.Label8 = New System.Windows.Forms.Label
        Me.Label7 = New System.Windows.Forms.Label
        Me.txtFiber = New System.Windows.Forms.TextBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label5 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
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
        Me.cmdRead.Location = New System.Drawing.Point(9, 148)
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
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(183, 295)
        Me.GroupBox1.TabIndex = 3
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications Setup"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.lblIntensity)
        Me.GroupBox2.Controls.Add(Me.lblSat)
        Me.GroupBox2.Controls.Add(Me.lblHue)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Controls.Add(Me.Label7)
        Me.GroupBox2.Controls.Add(Me.txtFiber)
        Me.GroupBox2.Controls.Add(Me.Label6)
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
        'lblIntensity
        '
        Me.lblIntensity.BackColor = System.Drawing.Color.White
        Me.lblIntensity.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lblIntensity.Location = New System.Drawing.Point(113, 256)
        Me.lblIntensity.Name = "lblIntensity"
        Me.lblIntensity.Size = New System.Drawing.Size(54, 20)
        Me.lblIntensity.TabIndex = 15
        Me.lblIntensity.Text = "0"
        Me.lblIntensity.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'lblSat
        '
        Me.lblSat.BackColor = System.Drawing.Color.White
        Me.lblSat.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lblSat.Location = New System.Drawing.Point(113, 233)
        Me.lblSat.Name = "lblSat"
        Me.lblSat.Size = New System.Drawing.Size(54, 20)
        Me.lblSat.TabIndex = 14
        Me.lblSat.Text = "0"
        Me.lblSat.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'lblHue
        '
        Me.lblHue.BackColor = System.Drawing.Color.White
        Me.lblHue.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lblHue.Location = New System.Drawing.Point(113, 210)
        Me.lblHue.Name = "lblHue"
        Me.lblHue.Size = New System.Drawing.Size(54, 20)
        Me.lblHue.TabIndex = 13
        Me.lblHue.Text = "0.0"
        Me.lblHue.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label8.Location = New System.Drawing.Point(59, 256)
        Me.Label8.Name = "Label8"
        Me.Label8.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Label8.Size = New System.Drawing.Size(54, 15)
        Me.Label8.TabIndex = 12
        Me.Label8.Text = "Intensity:"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label7.Location = New System.Drawing.Point(47, 233)
        Me.Label7.Name = "Label7"
        Me.Label7.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Label7.Size = New System.Drawing.Size(66, 15)
        Me.Label7.TabIndex = 10
        Me.Label7.Text = "Saturation:"
        '
        'txtFiber
        '
        Me.txtFiber.Location = New System.Drawing.Point(86, 113)
        Me.txtFiber.MaxLength = 2
        Me.txtFiber.Name = "txtFiber"
        Me.txtFiber.Size = New System.Drawing.Size(30, 20)
        Me.txtFiber.TabIndex = 9
        Me.txtFiber.Text = "1"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(6, 116)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(74, 13)
        Me.Label6.TabIndex = 8
        Me.Label6.Text = "Fiber to Read:"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(80, 210)
        Me.Label5.Name = "Label5"
        Me.Label5.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Label5.Size = New System.Drawing.Size(33, 15)
        Me.Label5.TabIndex = 6
        Me.Label5.Text = "Hue:"
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
        'Label3
        '
        Me.Label3.Location = New System.Drawing.Point(6, 208)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(171, 84)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "In this example we will retrieve the Hue, Saturation and intensity values and we " & _
            "will decompose them to show the values on the screen."
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
        Me.Text = "Get HSI simple - (c) Feasa Enterprises Ltd"
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
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents txtFiber As System.Windows.Forms.TextBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents lblIntensity As System.Windows.Forms.Label
    Friend WithEvents lblSat As System.Windows.Forms.Label
    Friend WithEvents lblHue As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label

End Class
