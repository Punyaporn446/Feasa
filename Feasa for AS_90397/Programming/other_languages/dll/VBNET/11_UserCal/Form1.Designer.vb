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
        Me.optRAM = New System.Windows.Forms.RadioButton()
        Me.lstCapture = New System.Windows.Forms.ComboBox()
        Me.btnReadParams = New System.Windows.Forms.Button()
        Me.txtRefy = New System.Windows.Forms.TextBox()
        Me.txtRefx = New System.Windows.Forms.TextBox()
        Me.label7 = New System.Windows.Forms.Label()
        Me.txtRefWavelength = New System.Windows.Forms.TextBox()
        Me.label6 = New System.Windows.Forms.Label()
        Me.chkPWM = New System.Windows.Forms.CheckBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.txtRefAbsInt = New System.Windows.Forms.TextBox()
        Me.label4 = New System.Windows.Forms.Label()
        Me.btnAdjustWavelength = New System.Windows.Forms.Button()
        Me.label2 = New System.Windows.Forms.Label()
        Me.lstPorts = New System.Windows.Forms.ComboBox()
        Me.btnAdjustxy = New System.Windows.Forms.Button()
        Me.numFibers = New System.Windows.Forms.NumericUpDown()
        Me.btnAdjustAbsInt = New System.Windows.Forms.Button()
        Me.label8 = New System.Windows.Forms.Label()
        Me.txtLog = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.btnBalanceInt = New System.Windows.Forms.Button()
        Me.label3 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.optFlash = New System.Windows.Forms.RadioButton()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.txtRefAbsIntB = New System.Windows.Forms.TextBox()
        Me.txtRefyB = New System.Windows.Forms.TextBox()
        Me.txtRefxB = New System.Windows.Forms.TextBox()
        Me.label11 = New System.Windows.Forms.Label()
        Me.txtRefAbsIntG = New System.Windows.Forms.TextBox()
        Me.txtRefyG = New System.Windows.Forms.TextBox()
        Me.txtRefxG = New System.Windows.Forms.TextBox()
        Me.label10 = New System.Windows.Forms.Label()
        Me.txtRefAbsIntR = New System.Windows.Forms.TextBox()
        Me.txtRefyR = New System.Windows.Forms.TextBox()
        Me.txtRefxR = New System.Windows.Forms.TextBox()
        Me.label9 = New System.Windows.Forms.Label()
        Me.btnAdjustRGB = New System.Windows.Forms.Button()
        CType(Me.numFibers, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'optRAM
        '
        Me.optRAM.AutoSize = True
        Me.optRAM.Location = New System.Drawing.Point(14, 257)
        Me.optRAM.Name = "optRAM"
        Me.optRAM.Size = New System.Drawing.Size(49, 17)
        Me.optRAM.TabIndex = 10
        Me.optRAM.TabStop = True
        Me.optRAM.Text = "RAM"
        Me.optRAM.UseVisualStyleBackColor = True
        '
        'lstCapture
        '
        Me.lstCapture.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstCapture.FormattingEnabled = True
        Me.lstCapture.Items.AddRange(New Object() {"AUTO", "LOW", "MEDIUM", "HIGH", "SUPER", "ULTRA"})
        Me.lstCapture.Location = New System.Drawing.Point(9, 167)
        Me.lstCapture.Name = "lstCapture"
        Me.lstCapture.Size = New System.Drawing.Size(135, 21)
        Me.lstCapture.TabIndex = 8
        '
        'btnReadParams
        '
        Me.btnReadParams.Location = New System.Drawing.Point(17, 411)
        Me.btnReadParams.Name = "btnReadParams"
        Me.btnReadParams.Size = New System.Drawing.Size(159, 27)
        Me.btnReadParams.TabIndex = 14
        Me.btnReadParams.Text = "Read back params"
        Me.btnReadParams.UseVisualStyleBackColor = True
        '
        'txtRefy
        '
        Me.txtRefy.Location = New System.Drawing.Point(112, 207)
        Me.txtRefy.MaxLength = 6
        Me.txtRefy.Name = "txtRefy"
        Me.txtRefy.Size = New System.Drawing.Size(64, 20)
        Me.txtRefy.TabIndex = 13
        Me.txtRefy.Text = "0.2912"
        '
        'txtRefx
        '
        Me.txtRefx.Location = New System.Drawing.Point(48, 207)
        Me.txtRefx.MaxLength = 6
        Me.txtRefx.Name = "txtRefx"
        Me.txtRefx.Size = New System.Drawing.Size(58, 20)
        Me.txtRefx.TabIndex = 12
        Me.txtRefx.Text = "0.7011"
        '
        'label7
        '
        Me.label7.AutoSize = True
        Me.label7.Location = New System.Drawing.Point(21, 210)
        Me.label7.Name = "label7"
        Me.label7.Size = New System.Drawing.Size(27, 13)
        Me.label7.TabIndex = 11
        Me.label7.Text = "Ref:"
        '
        'txtRefWavelength
        '
        Me.txtRefWavelength.Location = New System.Drawing.Point(59, 136)
        Me.txtRefWavelength.MaxLength = 3
        Me.txtRefWavelength.Name = "txtRefWavelength"
        Me.txtRefWavelength.Size = New System.Drawing.Size(47, 20)
        Me.txtRefWavelength.TabIndex = 10
        Me.txtRefWavelength.Text = "638"
        '
        'label6
        '
        Me.label6.AutoSize = True
        Me.label6.Location = New System.Drawing.Point(26, 140)
        Me.label6.Name = "label6"
        Me.label6.Size = New System.Drawing.Size(27, 13)
        Me.label6.TabIndex = 9
        Me.label6.Text = "Ref:"
        '
        'chkPWM
        '
        Me.chkPWM.AutoSize = True
        Me.chkPWM.Location = New System.Drawing.Point(9, 194)
        Me.chkPWM.Name = "chkPWM"
        Me.chkPWM.Size = New System.Drawing.Size(53, 17)
        Me.chkPWM.TabIndex = 7
        Me.chkPWM.Text = "PWM"
        Me.chkPWM.UseVisualStyleBackColor = True
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
        'txtRefAbsInt
        '
        Me.txtRefAbsInt.Location = New System.Drawing.Point(59, 65)
        Me.txtRefAbsInt.MaxLength = 11
        Me.txtRefAbsInt.Name = "txtRefAbsInt"
        Me.txtRefAbsInt.Size = New System.Drawing.Size(117, 20)
        Me.txtRefAbsInt.TabIndex = 8
        Me.txtRefAbsInt.Text = "2.355E-02"
        '
        'label4
        '
        Me.label4.AutoSize = True
        Me.label4.Location = New System.Drawing.Point(26, 68)
        Me.label4.Name = "label4"
        Me.label4.Size = New System.Drawing.Size(27, 13)
        Me.label4.TabIndex = 7
        Me.label4.Text = "Ref:"
        '
        'btnAdjustWavelength
        '
        Me.btnAdjustWavelength.Location = New System.Drawing.Point(17, 162)
        Me.btnAdjustWavelength.Name = "btnAdjustWavelength"
        Me.btnAdjustWavelength.Size = New System.Drawing.Size(159, 27)
        Me.btnAdjustWavelength.TabIndex = 3
        Me.btnAdjustWavelength.Text = "Adjust Wavelength"
        Me.btnAdjustWavelength.UseVisualStyleBackColor = True
        '
        'label2
        '
        Me.label2.AutoSize = True
        Me.label2.Location = New System.Drawing.Point(9, 147)
        Me.label2.Name = "label2"
        Me.label2.Size = New System.Drawing.Size(47, 13)
        Me.label2.TabIndex = 9
        Me.label2.Text = "Capture:"
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
        'btnAdjustxy
        '
        Me.btnAdjustxy.Location = New System.Drawing.Point(17, 233)
        Me.btnAdjustxy.Name = "btnAdjustxy"
        Me.btnAdjustxy.Size = New System.Drawing.Size(159, 27)
        Me.btnAdjustxy.TabIndex = 4
        Me.btnAdjustxy.Text = "Adjust xy"
        Me.btnAdjustxy.UseVisualStyleBackColor = True
        '
        'numFibers
        '
        Me.numFibers.Location = New System.Drawing.Point(78, 104)
        Me.numFibers.Maximum = New Decimal(New Integer() {20, 0, 0, 0})
        Me.numFibers.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numFibers.Name = "numFibers"
        Me.numFibers.Size = New System.Drawing.Size(48, 20)
        Me.numFibers.TabIndex = 5
        Me.numFibers.Value = New Decimal(New Integer() {10, 0, 0, 0})
        '
        'btnAdjustAbsInt
        '
        Me.btnAdjustAbsInt.Location = New System.Drawing.Point(17, 91)
        Me.btnAdjustAbsInt.Name = "btnAdjustAbsInt"
        Me.btnAdjustAbsInt.Size = New System.Drawing.Size(159, 27)
        Me.btnAdjustAbsInt.TabIndex = 2
        Me.btnAdjustAbsInt.Text = "Adjust Abs Int"
        Me.btnAdjustAbsInt.UseVisualStyleBackColor = True
        '
        'label8
        '
        Me.label8.AutoSize = True
        Me.label8.Location = New System.Drawing.Point(11, 241)
        Me.label8.Name = "label8"
        Me.label8.Size = New System.Drawing.Size(102, 13)
        Me.label8.TabIndex = 12
        Me.label8.Text = "Save parameters to:"
        '
        'txtLog
        '
        Me.txtLog.Location = New System.Drawing.Point(399, 30)
        Me.txtLog.Multiline = True
        Me.txtLog.Name = "txtLog"
        Me.txtLog.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.txtLog.Size = New System.Drawing.Size(251, 435)
        Me.txtLog.TabIndex = 11
        '
        'Label5
        '
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(396, 5)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(162, 22)
        Me.Label5.TabIndex = 9
        Me.Label5.Text = "Results:"
        '
        'btnBalanceInt
        '
        Me.btnBalanceInt.Location = New System.Drawing.Point(17, 20)
        Me.btnBalanceInt.Name = "btnBalanceInt"
        Me.btnBalanceInt.Size = New System.Drawing.Size(159, 27)
        Me.btnBalanceInt.TabIndex = 1
        Me.btnBalanceInt.Text = "Balance Int to avg"
        Me.btnBalanceInt.UseVisualStyleBackColor = True
        '
        'label3
        '
        Me.label3.AutoSize = True
        Me.label3.Location = New System.Drawing.Point(9, 106)
        Me.label3.Name = "label3"
        Me.label3.Size = New System.Drawing.Size(63, 13)
        Me.label3.TabIndex = 6
        Me.label3.Text = "Fiber count:"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.txtRefAbsIntB)
        Me.GroupBox2.Controls.Add(Me.txtRefyB)
        Me.GroupBox2.Controls.Add(Me.txtRefxB)
        Me.GroupBox2.Controls.Add(Me.label11)
        Me.GroupBox2.Controls.Add(Me.txtRefAbsIntG)
        Me.GroupBox2.Controls.Add(Me.txtRefyG)
        Me.GroupBox2.Controls.Add(Me.txtRefxG)
        Me.GroupBox2.Controls.Add(Me.label10)
        Me.GroupBox2.Controls.Add(Me.txtRefAbsIntR)
        Me.GroupBox2.Controls.Add(Me.txtRefyR)
        Me.GroupBox2.Controls.Add(Me.txtRefxR)
        Me.GroupBox2.Controls.Add(Me.label9)
        Me.GroupBox2.Controls.Add(Me.btnAdjustRGB)
        Me.GroupBox2.Controls.Add(Me.btnReadParams)
        Me.GroupBox2.Controls.Add(Me.txtRefy)
        Me.GroupBox2.Controls.Add(Me.txtRefx)
        Me.GroupBox2.Controls.Add(Me.label7)
        Me.GroupBox2.Controls.Add(Me.txtRefWavelength)
        Me.GroupBox2.Controls.Add(Me.label6)
        Me.GroupBox2.Controls.Add(Me.txtRefAbsInt)
        Me.GroupBox2.Controls.Add(Me.label4)
        Me.GroupBox2.Controls.Add(Me.btnAdjustxy)
        Me.GroupBox2.Controls.Add(Me.btnAdjustWavelength)
        Me.GroupBox2.Controls.Add(Me.btnAdjustAbsInt)
        Me.GroupBox2.Controls.Add(Me.btnBalanceInt)
        Me.GroupBox2.Location = New System.Drawing.Point(201, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(192, 454)
        Me.GroupBox2.TabIndex = 10
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Commands"
        '
        'optFlash
        '
        Me.optFlash.AutoSize = True
        Me.optFlash.Location = New System.Drawing.Point(69, 257)
        Me.optFlash.Name = "optFlash"
        Me.optFlash.Size = New System.Drawing.Size(50, 17)
        Me.optFlash.TabIndex = 11
        Me.optFlash.TabStop = True
        Me.optFlash.Text = "Flash"
        Me.optFlash.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.label8)
        Me.GroupBox1.Controls.Add(Me.optFlash)
        Me.GroupBox1.Controls.Add(Me.optRAM)
        Me.GroupBox1.Controls.Add(Me.label2)
        Me.GroupBox1.Controls.Add(Me.lstCapture)
        Me.GroupBox1.Controls.Add(Me.chkPWM)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Controls.Add(Me.label3)
        Me.GroupBox1.Controls.Add(Me.numFibers)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(183, 454)
        Me.GroupBox1.TabIndex = 8
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications Setup"
        '
        'txtRefAbsIntB
        '
        Me.txtRefAbsIntB.Location = New System.Drawing.Point(121, 335)
        Me.txtRefAbsIntB.MaxLength = 6
        Me.txtRefAbsIntB.Name = "txtRefAbsIntB"
        Me.txtRefAbsIntB.Size = New System.Drawing.Size(55, 20)
        Me.txtRefAbsIntB.TabIndex = 54
        Me.txtRefAbsIntB.Text = "0.550639"
        '
        'txtRefyB
        '
        Me.txtRefyB.Location = New System.Drawing.Point(77, 335)
        Me.txtRefyB.MaxLength = 6
        Me.txtRefyB.Name = "txtRefyB"
        Me.txtRefyB.Size = New System.Drawing.Size(41, 20)
        Me.txtRefyB.TabIndex = 53
        Me.txtRefyB.Text = "0.0388"
        '
        'txtRefxB
        '
        Me.txtRefxB.Location = New System.Drawing.Point(34, 335)
        Me.txtRefxB.MaxLength = 6
        Me.txtRefxB.Name = "txtRefxB"
        Me.txtRefxB.Size = New System.Drawing.Size(40, 20)
        Me.txtRefxB.TabIndex = 52
        Me.txtRefxB.Text = "0.1427"
        '
        'label11
        '
        Me.label11.AutoSize = True
        Me.label11.Location = New System.Drawing.Point(17, 338)
        Me.label11.Name = "label11"
        Me.label11.Size = New System.Drawing.Size(17, 13)
        Me.label11.TabIndex = 51
        Me.label11.Text = "B:"
        '
        'txtRefAbsIntG
        '
        Me.txtRefAbsIntG.Location = New System.Drawing.Point(121, 309)
        Me.txtRefAbsIntG.MaxLength = 6
        Me.txtRefAbsIntG.Name = "txtRefAbsIntG"
        Me.txtRefAbsIntG.Size = New System.Drawing.Size(55, 20)
        Me.txtRefAbsIntG.TabIndex = 50
        Me.txtRefAbsIntG.Text = "2.20375"
        '
        'txtRefyG
        '
        Me.txtRefyG.Location = New System.Drawing.Point(77, 309)
        Me.txtRefyG.MaxLength = 6
        Me.txtRefyG.Name = "txtRefyG"
        Me.txtRefyG.Size = New System.Drawing.Size(41, 20)
        Me.txtRefyG.TabIndex = 49
        Me.txtRefyG.Text = "0.7177"
        '
        'txtRefxG
        '
        Me.txtRefxG.Location = New System.Drawing.Point(34, 309)
        Me.txtRefxG.MaxLength = 6
        Me.txtRefxG.Name = "txtRefxG"
        Me.txtRefxG.Size = New System.Drawing.Size(40, 20)
        Me.txtRefxG.TabIndex = 48
        Me.txtRefxG.Text = "0.1827"
        '
        'label10
        '
        Me.label10.AutoSize = True
        Me.label10.Location = New System.Drawing.Point(17, 312)
        Me.label10.Name = "label10"
        Me.label10.Size = New System.Drawing.Size(18, 13)
        Me.label10.TabIndex = 47
        Me.label10.Text = "G:"
        '
        'txtRefAbsIntR
        '
        Me.txtRefAbsIntR.Location = New System.Drawing.Point(121, 283)
        Me.txtRefAbsIntR.MaxLength = 6
        Me.txtRefAbsIntR.Name = "txtRefAbsIntR"
        Me.txtRefAbsIntR.Size = New System.Drawing.Size(55, 20)
        Me.txtRefAbsIntR.TabIndex = 46
        Me.txtRefAbsIntR.Text = "1.28569"
        '
        'txtRefyR
        '
        Me.txtRefyR.Location = New System.Drawing.Point(77, 283)
        Me.txtRefyR.MaxLength = 6
        Me.txtRefyR.Name = "txtRefyR"
        Me.txtRefyR.Size = New System.Drawing.Size(41, 20)
        Me.txtRefyR.TabIndex = 45
        Me.txtRefyR.Text = "0.2927"
        '
        'txtRefxR
        '
        Me.txtRefxR.Location = New System.Drawing.Point(34, 283)
        Me.txtRefxR.MaxLength = 6
        Me.txtRefxR.Name = "txtRefxR"
        Me.txtRefxR.Size = New System.Drawing.Size(40, 20)
        Me.txtRefxR.TabIndex = 44
        Me.txtRefxR.Text = "0.7071"
        '
        'label9
        '
        Me.label9.AutoSize = True
        Me.label9.Location = New System.Drawing.Point(17, 286)
        Me.label9.Name = "label9"
        Me.label9.Size = New System.Drawing.Size(18, 13)
        Me.label9.TabIndex = 43
        Me.label9.Text = "R:"
        '
        'btnAdjustRGB
        '
        Me.btnAdjustRGB.Location = New System.Drawing.Point(17, 363)
        Me.btnAdjustRGB.Name = "btnAdjustRGB"
        Me.btnAdjustRGB.Size = New System.Drawing.Size(159, 27)
        Me.btnAdjustRGB.TabIndex = 42
        Me.btnAdjustRGB.Text = "Adjust RGB"
        Me.btnAdjustRGB.UseVisualStyleBackColor = True
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(668, 477)
        Me.Controls.Add(Me.txtLog)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "UserCal demo - (c) Feasa Enterprises Ltd"
        CType(Me.numFibers, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Private WithEvents optRAM As RadioButton
    Private WithEvents lstCapture As ComboBox
    Friend WithEvents btnReadParams As Button
    Private WithEvents txtRefy As TextBox
    Private WithEvents txtRefx As TextBox
    Private WithEvents label7 As Label
    Private WithEvents txtRefWavelength As TextBox
    Private WithEvents label6 As Label
    Private WithEvents chkPWM As CheckBox
    Friend WithEvents Label1 As Label
    Private WithEvents txtRefAbsInt As TextBox
    Private WithEvents label4 As Label
    Friend WithEvents btnAdjustWavelength As Button
    Private WithEvents label2 As Label
    Friend WithEvents lstPorts As ComboBox
    Friend WithEvents btnAdjustxy As Button
    Private WithEvents numFibers As NumericUpDown
    Friend WithEvents btnAdjustAbsInt As Button
    Private WithEvents label8 As Label
    Friend WithEvents txtLog As TextBox
    Friend WithEvents Label5 As Label
    Friend WithEvents btnBalanceInt As Button
    Private WithEvents label3 As Label
    Friend WithEvents GroupBox2 As GroupBox
    Private WithEvents optFlash As RadioButton
    Friend WithEvents GroupBox1 As GroupBox
    Private WithEvents txtRefAbsIntB As TextBox
    Private WithEvents txtRefyB As TextBox
    Private WithEvents txtRefxB As TextBox
    Private WithEvents label11 As Label
    Private WithEvents txtRefAbsIntG As TextBox
    Private WithEvents txtRefyG As TextBox
    Private WithEvents txtRefxG As TextBox
    Private WithEvents label10 As Label
    Private WithEvents txtRefAbsIntR As TextBox
    Private WithEvents txtRefyR As TextBox
    Private WithEvents txtRefxR As TextBox
    Private WithEvents label9 As Label
    Friend WithEvents btnAdjustRGB As Button
End Class
