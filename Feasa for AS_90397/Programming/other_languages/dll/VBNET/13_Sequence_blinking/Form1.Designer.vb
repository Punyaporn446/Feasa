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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.groupBox4 = New System.Windows.Forms.GroupBox()
        Me.numFiber = New System.Windows.Forms.NumericUpDown()
        Me.btnSequenceTest = New System.Windows.Forms.Button()
        Me.numCaptureTime = New System.Windows.Forms.NumericUpDown()
        Me.label12 = New System.Windows.Forms.Label()
        Me.numSampleCount = New System.Windows.Forms.NumericUpDown()
        Me.numWaitTime = New System.Windows.Forms.NumericUpDown()
        Me.label11 = New System.Windows.Forms.Label()
        Me.label10 = New System.Windows.Forms.Label()
        Me.numStartDelay = New System.Windows.Forms.NumericUpDown()
        Me.label9 = New System.Windows.Forms.Label()
        Me.label8 = New System.Windows.Forms.Label()
        Me.btnFindParams = New System.Windows.Forms.Button()
        Me.label7 = New System.Windows.Forms.Label()
        Me.label5 = New System.Windows.Forms.Label()
        Me.chkTimeResImportant = New System.Windows.Forms.CheckBox()
        Me.numLEDCount = New System.Windows.Forms.NumericUpDown()
        Me.numFiberToTest = New System.Windows.Forms.NumericUpDown()
        Me.numCycles = New System.Windows.Forms.NumericUpDown()
        Me.label4 = New System.Windows.Forms.Label()
        Me.lstSignalSpeed = New System.Windows.Forms.ComboBox()
        Me.label2 = New System.Windows.Forms.Label()
        Me.lstBlinkingSpeed = New System.Windows.Forms.ComboBox()
        Me.label3 = New System.Windows.Forms.Label()
        Me.groupBox3 = New System.Windows.Forms.GroupBox()
        Me.label6 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.lstPorts = New System.Windows.Forms.ComboBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.graphCIE = New ZedGraph.ZedGraphControl()
        Me.graphInt = New ZedGraph.ZedGraphControl()
        Me.groupBox4.SuspendLayout()
        CType(Me.numFiber, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numCaptureTime, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numSampleCount, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numWaitTime, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numStartDelay, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numLEDCount, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numFiberToTest, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numCycles, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.groupBox3.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'groupBox4
        '
        Me.groupBox4.Controls.Add(Me.numFiber)
        Me.groupBox4.Controls.Add(Me.btnSequenceTest)
        Me.groupBox4.Controls.Add(Me.numCaptureTime)
        Me.groupBox4.Controls.Add(Me.label12)
        Me.groupBox4.Controls.Add(Me.numSampleCount)
        Me.groupBox4.Controls.Add(Me.numWaitTime)
        Me.groupBox4.Controls.Add(Me.label11)
        Me.groupBox4.Controls.Add(Me.label10)
        Me.groupBox4.Controls.Add(Me.numStartDelay)
        Me.groupBox4.Controls.Add(Me.label9)
        Me.groupBox4.Controls.Add(Me.label8)
        Me.groupBox4.Location = New System.Drawing.Point(557, 12)
        Me.groupBox4.Name = "groupBox4"
        Me.groupBox4.Size = New System.Drawing.Size(180, 258)
        Me.groupBox4.TabIndex = 29
        Me.groupBox4.TabStop = False
        Me.groupBox4.Text = "Sequence Test"
        '
        'numFiber
        '
        Me.numFiber.Location = New System.Drawing.Point(103, 33)
        Me.numFiber.Maximum = New Decimal(New Integer() {20, 0, 0, 0})
        Me.numFiber.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numFiber.Name = "numFiber"
        Me.numFiber.Size = New System.Drawing.Size(64, 20)
        Me.numFiber.TabIndex = 23
        Me.numFiber.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'btnSequenceTest
        '
        Me.btnSequenceTest.Location = New System.Drawing.Point(16, 189)
        Me.btnSequenceTest.Name = "btnSequenceTest"
        Me.btnSequenceTest.Size = New System.Drawing.Size(151, 43)
        Me.btnSequenceTest.TabIndex = 25
        Me.btnSequenceTest.Text = "SEQUENCE TEST"
        Me.btnSequenceTest.UseVisualStyleBackColor = True
        '
        'numCaptureTime
        '
        Me.numCaptureTime.Location = New System.Drawing.Point(103, 85)
        Me.numCaptureTime.Maximum = New Decimal(New Integer() {999, 0, 0, 0})
        Me.numCaptureTime.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numCaptureTime.Name = "numCaptureTime"
        Me.numCaptureTime.Size = New System.Drawing.Size(64, 20)
        Me.numCaptureTime.TabIndex = 15
        Me.numCaptureTime.Value = New Decimal(New Integer() {4, 0, 0, 0})
        '
        'label12
        '
        Me.label12.AutoSize = True
        Me.label12.Location = New System.Drawing.Point(30, 35)
        Me.label12.Name = "label12"
        Me.label12.Size = New System.Drawing.Size(65, 13)
        Me.label12.TabIndex = 24
        Me.label12.Text = "Fiber to test:"
        Me.label12.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'numSampleCount
        '
        Me.numSampleCount.Location = New System.Drawing.Point(103, 137)
        Me.numSampleCount.Maximum = New Decimal(New Integer() {9999, 0, 0, 0})
        Me.numSampleCount.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numSampleCount.Name = "numSampleCount"
        Me.numSampleCount.Size = New System.Drawing.Size(64, 20)
        Me.numSampleCount.TabIndex = 16
        Me.numSampleCount.Value = New Decimal(New Integer() {200, 0, 0, 0})
        '
        'numWaitTime
        '
        Me.numWaitTime.Location = New System.Drawing.Point(103, 111)
        Me.numWaitTime.Maximum = New Decimal(New Integer() {999, 0, 0, 0})
        Me.numWaitTime.Name = "numWaitTime"
        Me.numWaitTime.Size = New System.Drawing.Size(64, 20)
        Me.numWaitTime.TabIndex = 17
        '
        'label11
        '
        Me.label11.AutoSize = True
        Me.label11.Location = New System.Drawing.Point(30, 61)
        Me.label11.Name = "label11"
        Me.label11.Size = New System.Drawing.Size(60, 13)
        Me.label11.TabIndex = 22
        Me.label11.Text = "Start delay:"
        Me.label11.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'label10
        '
        Me.label10.AutoSize = True
        Me.label10.Location = New System.Drawing.Point(20, 87)
        Me.label10.Name = "label10"
        Me.label10.Size = New System.Drawing.Size(69, 13)
        Me.label10.TabIndex = 18
        Me.label10.Text = "Capture time:"
        Me.label10.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'numStartDelay
        '
        Me.numStartDelay.Location = New System.Drawing.Point(103, 59)
        Me.numStartDelay.Maximum = New Decimal(New Integer() {999, 0, 0, 0})
        Me.numStartDelay.Name = "numStartDelay"
        Me.numStartDelay.Size = New System.Drawing.Size(64, 20)
        Me.numStartDelay.TabIndex = 21
        '
        'label9
        '
        Me.label9.AutoSize = True
        Me.label9.Location = New System.Drawing.Point(41, 113)
        Me.label9.Name = "label9"
        Me.label9.Size = New System.Drawing.Size(54, 13)
        Me.label9.TabIndex = 19
        Me.label9.Text = "Wait time:"
        Me.label9.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'label8
        '
        Me.label8.AutoSize = True
        Me.label8.Location = New System.Drawing.Point(20, 139)
        Me.label8.Name = "label8"
        Me.label8.Size = New System.Drawing.Size(75, 13)
        Me.label8.TabIndex = 20
        Me.label8.Text = "Sample count:"
        Me.label8.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'btnFindParams
        '
        Me.btnFindParams.Location = New System.Drawing.Point(18, 189)
        Me.btnFindParams.Name = "btnFindParams"
        Me.btnFindParams.Size = New System.Drawing.Size(233, 43)
        Me.btnFindParams.TabIndex = 18
        Me.btnFindParams.Text = "FIND PARAMETERS"
        Me.btnFindParams.UseVisualStyleBackColor = True
        '
        'label7
        '
        Me.label7.AutoSize = True
        Me.label7.Location = New System.Drawing.Point(114, 135)
        Me.label7.Name = "label7"
        Me.label7.Size = New System.Drawing.Size(65, 13)
        Me.label7.TabIndex = 13
        Me.label7.Text = "Fiber to test:"
        Me.label7.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'label5
        '
        Me.label5.AutoSize = True
        Me.label5.Location = New System.Drawing.Point(68, 83)
        Me.label5.Name = "label5"
        Me.label5.Size = New System.Drawing.Size(111, 13)
        Me.label5.TabIndex = 11
        Me.label5.Text = "Min cycles to capture:"
        Me.label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'chkTimeResImportant
        '
        Me.chkTimeResImportant.AutoSize = True
        Me.chkTimeResImportant.Checked = True
        Me.chkTimeResImportant.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkTimeResImportant.Location = New System.Drawing.Point(18, 166)
        Me.chkTimeResImportant.Name = "chkTimeResImportant"
        Me.chkTimeResImportant.Size = New System.Drawing.Size(153, 17)
        Me.chkTimeResImportant.TabIndex = 10
        Me.chkTimeResImportant.Text = "Time resolution is important"
        Me.chkTimeResImportant.UseVisualStyleBackColor = True
        '
        'numLEDCount
        '
        Me.numLEDCount.Location = New System.Drawing.Point(187, 107)
        Me.numLEDCount.Maximum = New Decimal(New Integer() {20, 0, 0, 0})
        Me.numLEDCount.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numLEDCount.Name = "numLEDCount"
        Me.numLEDCount.Size = New System.Drawing.Size(64, 20)
        Me.numLEDCount.TabIndex = 9
        Me.numLEDCount.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'numFiberToTest
        '
        Me.numFiberToTest.Location = New System.Drawing.Point(187, 133)
        Me.numFiberToTest.Maximum = New Decimal(New Integer() {20, 0, 0, 0})
        Me.numFiberToTest.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numFiberToTest.Name = "numFiberToTest"
        Me.numFiberToTest.Size = New System.Drawing.Size(64, 20)
        Me.numFiberToTest.TabIndex = 8
        Me.numFiberToTest.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'numCycles
        '
        Me.numCycles.Location = New System.Drawing.Point(187, 81)
        Me.numCycles.Maximum = New Decimal(New Integer() {50, 0, 0, 0})
        Me.numCycles.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numCycles.Name = "numCycles"
        Me.numCycles.Size = New System.Drawing.Size(64, 20)
        Me.numCycles.TabIndex = 7
        Me.numCycles.Value = New Decimal(New Integer() {4, 0, 0, 0})
        '
        'label4
        '
        Me.label4.AutoSize = True
        Me.label4.Location = New System.Drawing.Point(15, 57)
        Me.label4.Name = "label4"
        Me.label4.Size = New System.Drawing.Size(73, 13)
        Me.label4.TabIndex = 5
        Me.label4.Text = "Signal Speed:"
        '
        'lstSignalSpeed
        '
        Me.lstSignalSpeed.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstSignalSpeed.FormattingEnabled = True
        Me.lstSignalSpeed.Location = New System.Drawing.Point(102, 54)
        Me.lstSignalSpeed.Name = "lstSignalSpeed"
        Me.lstSignalSpeed.Size = New System.Drawing.Size(149, 21)
        Me.lstSignalSpeed.TabIndex = 6
        '
        'label2
        '
        Me.label2.AutoSize = True
        Me.label2.Location = New System.Drawing.Point(15, 30)
        Me.label2.Name = "label2"
        Me.label2.Size = New System.Drawing.Size(81, 13)
        Me.label2.TabIndex = 3
        Me.label2.Text = "Blinking Speed:"
        '
        'lstBlinkingSpeed
        '
        Me.lstBlinkingSpeed.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstBlinkingSpeed.FormattingEnabled = True
        Me.lstBlinkingSpeed.Location = New System.Drawing.Point(102, 27)
        Me.lstBlinkingSpeed.Name = "lstBlinkingSpeed"
        Me.lstBlinkingSpeed.Size = New System.Drawing.Size(149, 21)
        Me.lstBlinkingSpeed.TabIndex = 4
        '
        'label3
        '
        Me.label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.label3.Location = New System.Drawing.Point(18, 61)
        Me.label3.Name = "label3"
        Me.label3.Size = New System.Drawing.Size(232, 184)
        Me.label3.TabIndex = 20
        Me.label3.Text = resources.GetString("label3.Text")
        '
        'groupBox3
        '
        Me.groupBox3.Controls.Add(Me.btnFindParams)
        Me.groupBox3.Controls.Add(Me.label7)
        Me.groupBox3.Controls.Add(Me.label6)
        Me.groupBox3.Controls.Add(Me.label5)
        Me.groupBox3.Controls.Add(Me.chkTimeResImportant)
        Me.groupBox3.Controls.Add(Me.numLEDCount)
        Me.groupBox3.Controls.Add(Me.numFiberToTest)
        Me.groupBox3.Controls.Add(Me.numCycles)
        Me.groupBox3.Controls.Add(Me.label4)
        Me.groupBox3.Controls.Add(Me.lstSignalSpeed)
        Me.groupBox3.Controls.Add(Me.label2)
        Me.groupBox3.Controls.Add(Me.lstBlinkingSpeed)
        Me.groupBox3.Location = New System.Drawing.Point(289, 12)
        Me.groupBox3.Name = "groupBox3"
        Me.groupBox3.Size = New System.Drawing.Size(262, 258)
        Me.groupBox3.TabIndex = 28
        Me.groupBox3.TabStop = False
        Me.groupBox3.Text = "Find parameters"
        '
        'label6
        '
        Me.label6.AutoSize = True
        Me.label6.Location = New System.Drawing.Point(90, 109)
        Me.label6.Name = "label6"
        Me.label6.Size = New System.Drawing.Size(89, 13)
        Me.label6.TabIndex = 12
        Me.label6.Text = "Total LED Count:"
        Me.label6.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(15, 27)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(56, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "COM Port:"
        '
        'lstPorts
        '
        Me.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstPorts.FormattingEnabled = True
        Me.lstPorts.Location = New System.Drawing.Point(77, 24)
        Me.lstPorts.Name = "lstPorts"
        Me.lstPorts.Size = New System.Drawing.Size(123, 21)
        Me.lstPorts.TabIndex = 2
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.label3)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(271, 258)
        Me.GroupBox1.TabIndex = 27
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications setup"
        '
        'graphCIE
        '
        Me.graphCIE.IsEnableHPan = False
        Me.graphCIE.IsEnableHZoom = False
        Me.graphCIE.IsEnableVPan = False
        Me.graphCIE.IsEnableVZoom = False
        Me.graphCIE.IsEnableWheelZoom = False
        Me.graphCIE.Location = New System.Drawing.Point(12, 458)
        Me.graphCIE.Name = "graphCIE"
        Me.graphCIE.PanButtons = System.Windows.Forms.MouseButtons.None
        Me.graphCIE.PanButtons2 = System.Windows.Forms.MouseButtons.None
        Me.graphCIE.PanModifierKeys = System.Windows.Forms.Keys.None
        Me.graphCIE.ScrollGrace = 0R
        Me.graphCIE.ScrollMaxX = 0R
        Me.graphCIE.ScrollMaxY = 0R
        Me.graphCIE.ScrollMaxY2 = 0R
        Me.graphCIE.ScrollMinX = 0R
        Me.graphCIE.ScrollMinY = 0R
        Me.graphCIE.ScrollMinY2 = 0R
        Me.graphCIE.Size = New System.Drawing.Size(725, 176)
        Me.graphCIE.TabIndex = 31
        '
        'graphInt
        '
        Me.graphInt.IsEnableHPan = False
        Me.graphInt.IsEnableHZoom = False
        Me.graphInt.IsEnableVPan = False
        Me.graphInt.IsEnableVZoom = False
        Me.graphInt.IsEnableWheelZoom = False
        Me.graphInt.Location = New System.Drawing.Point(12, 276)
        Me.graphInt.Name = "graphInt"
        Me.graphInt.PanButtons = System.Windows.Forms.MouseButtons.None
        Me.graphInt.PanButtons2 = System.Windows.Forms.MouseButtons.None
        Me.graphInt.PanModifierKeys = System.Windows.Forms.Keys.None
        Me.graphInt.ScrollGrace = 0R
        Me.graphInt.ScrollMaxX = 0R
        Me.graphInt.ScrollMaxY = 0R
        Me.graphInt.ScrollMaxY2 = 0R
        Me.graphInt.ScrollMinX = 0R
        Me.graphInt.ScrollMinY = 0R
        Me.graphInt.ScrollMinY2 = 0R
        Me.graphInt.Size = New System.Drawing.Size(725, 176)
        Me.graphInt.TabIndex = 30
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(756, 649)
        Me.Controls.Add(Me.graphCIE)
        Me.Controls.Add(Me.graphInt)
        Me.Controls.Add(Me.groupBox4)
        Me.Controls.Add(Me.groupBox3)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Sequence - (c) Feasa Enterprises Ltd"
        Me.groupBox4.ResumeLayout(False)
        Me.groupBox4.PerformLayout()
        CType(Me.numFiber, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numCaptureTime, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numSampleCount, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numWaitTime, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numStartDelay, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numLEDCount, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numFiberToTest, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numCycles, System.ComponentModel.ISupportInitialize).EndInit()
        Me.groupBox3.ResumeLayout(False)
        Me.groupBox3.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Private WithEvents groupBox4 As GroupBox
    Private WithEvents numFiber As NumericUpDown
    Friend WithEvents btnSequenceTest As Button
    Private WithEvents numCaptureTime As NumericUpDown
    Friend WithEvents label12 As Label
    Private WithEvents numSampleCount As NumericUpDown
    Private WithEvents numWaitTime As NumericUpDown
    Friend WithEvents label11 As Label
    Friend WithEvents label10 As Label
    Private WithEvents numStartDelay As NumericUpDown
    Friend WithEvents label9 As Label
    Friend WithEvents label8 As Label
    Friend WithEvents btnFindParams As Button
    Friend WithEvents label7 As Label
    Friend WithEvents label5 As Label
    Private WithEvents chkTimeResImportant As CheckBox
    Private WithEvents numLEDCount As NumericUpDown
    Private WithEvents numFiberToTest As NumericUpDown
    Private WithEvents numCycles As NumericUpDown
    Friend WithEvents label4 As Label
    Friend WithEvents lstSignalSpeed As ComboBox
    Friend WithEvents label2 As Label
    Friend WithEvents lstBlinkingSpeed As ComboBox
    Friend WithEvents label3 As Label
    Private WithEvents groupBox3 As GroupBox
    Friend WithEvents label6 As Label
    Friend WithEvents Label1 As Label
    Friend WithEvents lstPorts As ComboBox
    Friend WithEvents GroupBox1 As GroupBox
    Private WithEvents graphCIE As ZedGraph.ZedGraphControl
    Private WithEvents graphInt As ZedGraph.ZedGraphControl
End Class
