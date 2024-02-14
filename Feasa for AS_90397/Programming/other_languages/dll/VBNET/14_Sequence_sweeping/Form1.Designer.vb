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
        Me.groupBox4 = New System.Windows.Forms.GroupBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.numFibers = New System.Windows.Forms.NumericUpDown()
        Me.btnSequenceTest = New System.Windows.Forms.Button()
        Me.numCaptureTime = New System.Windows.Forms.NumericUpDown()
        Me.label12 = New System.Windows.Forms.Label()
        Me.chkIsOffToOnPattern = New System.Windows.Forms.CheckBox()
        Me.numSampleCount = New System.Windows.Forms.NumericUpDown()
        Me.numWaitTime = New System.Windows.Forms.NumericUpDown()
        Me.label11 = New System.Windows.Forms.Label()
        Me.label10 = New System.Windows.Forms.Label()
        Me.numStartDelay = New System.Windows.Forms.NumericUpDown()
        Me.label9 = New System.Windows.Forms.Label()
        Me.label8 = New System.Windows.Forms.Label()
        Me.label3 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.lstPorts = New System.Windows.Forms.ComboBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.graphInt = New ZedGraph.ZedGraphControl()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.datagridTimes = New System.Windows.Forms.DataGridView()
        Me.colFib = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colLowTimes = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colHighTimes = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colIntensity = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.groupBox4.SuspendLayout()
        CType(Me.numFibers, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numCaptureTime, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numSampleCount, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numWaitTime, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.numStartDelay, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.datagridTimes, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'groupBox4
        '
        Me.groupBox4.Controls.Add(Me.Label2)
        Me.groupBox4.Controls.Add(Me.numFibers)
        Me.groupBox4.Controls.Add(Me.btnSequenceTest)
        Me.groupBox4.Controls.Add(Me.numCaptureTime)
        Me.groupBox4.Controls.Add(Me.label12)
        Me.groupBox4.Controls.Add(Me.chkIsOffToOnPattern)
        Me.groupBox4.Controls.Add(Me.numSampleCount)
        Me.groupBox4.Controls.Add(Me.numWaitTime)
        Me.groupBox4.Controls.Add(Me.label11)
        Me.groupBox4.Controls.Add(Me.label10)
        Me.groupBox4.Controls.Add(Me.numStartDelay)
        Me.groupBox4.Controls.Add(Me.label9)
        Me.groupBox4.Controls.Add(Me.label8)
        Me.groupBox4.Location = New System.Drawing.Point(227, 12)
        Me.groupBox4.Name = "groupBox4"
        Me.groupBox4.Size = New System.Drawing.Size(242, 258)
        Me.groupBox4.TabIndex = 29
        Me.groupBox4.TabStop = False
        Me.groupBox4.Text = "Sequence Test"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(173, 35)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(60, 13)
        Me.Label2.TabIndex = 26
        Me.Label2.Text = "(test 1 to n)"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'numFibers
        '
        Me.numFibers.Location = New System.Drawing.Point(103, 33)
        Me.numFibers.Maximum = New Decimal(New Integer() {20, 0, 0, 0})
        Me.numFibers.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.numFibers.Name = "numFibers"
        Me.numFibers.Size = New System.Drawing.Size(64, 20)
        Me.numFibers.TabIndex = 23
        Me.numFibers.Value = New Decimal(New Integer() {12, 0, 0, 0})
        '
        'btnSequenceTest
        '
        Me.btnSequenceTest.Location = New System.Drawing.Point(16, 189)
        Me.btnSequenceTest.Name = "btnSequenceTest"
        Me.btnSequenceTest.Size = New System.Drawing.Size(212, 43)
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
        Me.label12.Size = New System.Drawing.Size(70, 13)
        Me.label12.TabIndex = 24
        Me.label12.Text = "Fibers to test:"
        Me.label12.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        '
        'chkIsOffToOnPattern
        '
        Me.chkIsOffToOnPattern.AutoSize = True
        Me.chkIsOffToOnPattern.Checked = True
        Me.chkIsOffToOnPattern.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkIsOffToOnPattern.Location = New System.Drawing.Point(51, 163)
        Me.chkIsOffToOnPattern.Name = "chkIsOffToOnPattern"
        Me.chkIsOffToOnPattern.Size = New System.Drawing.Size(116, 17)
        Me.chkIsOffToOnPattern.TabIndex = 10
        Me.chkIsOffToOnPattern.Text = "Is Off-to-On pattern"
        Me.chkIsOffToOnPattern.UseVisualStyleBackColor = True
        '
        'numSampleCount
        '
        Me.numSampleCount.Increment = New Decimal(New Integer() {25, 0, 0, 0})
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
        'label3
        '
        Me.label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.label3.Location = New System.Drawing.Point(18, 61)
        Me.label3.Name = "label3"
        Me.label3.Size = New System.Drawing.Size(169, 184)
        Me.label3.TabIndex = 20
        Me.label3.Text = "This example explores the Sequence functionality applied to the more and more com" &
    "mon automotive sweeping indicators"
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
        Me.lstPorts.Size = New System.Drawing.Size(110, 21)
        Me.lstPorts.TabIndex = 2
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.label3)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(209, 258)
        Me.GroupBox1.TabIndex = 27
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications setup"
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
        Me.graphInt.Size = New System.Drawing.Size(725, 361)
        Me.graphInt.TabIndex = 30
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.datagridTimes)
        Me.GroupBox2.Location = New System.Drawing.Point(475, 12)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(271, 258)
        Me.GroupBox2.TabIndex = 32
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Timing"
        '
        'datagridTimes
        '
        Me.datagridTimes.AllowUserToAddRows = False
        Me.datagridTimes.AllowUserToDeleteRows = False
        Me.datagridTimes.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.datagridTimes.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.colFib, Me.colLowTimes, Me.colHighTimes, Me.colIntensity})
        Me.datagridTimes.Location = New System.Drawing.Point(9, 18)
        Me.datagridTimes.Name = "datagridTimes"
        Me.datagridTimes.ReadOnly = True
        Me.datagridTimes.RowHeadersVisible = False
        Me.datagridTimes.Size = New System.Drawing.Size(252, 226)
        Me.datagridTimes.TabIndex = 0
        '
        'colFib
        '
        Me.colFib.HeaderText = "Fib"
        Me.colFib.Name = "colFib"
        Me.colFib.ReadOnly = True
        Me.colFib.Width = 35
        '
        'colLowTimes
        '
        Me.colLowTimes.HeaderText = "Low (ms)"
        Me.colLowTimes.Name = "colLowTimes"
        Me.colLowTimes.ReadOnly = True
        Me.colLowTimes.Width = 60
        '
        'colHighTimes
        '
        Me.colHighTimes.HeaderText = "High (ms)"
        Me.colHighTimes.Name = "colHighTimes"
        Me.colHighTimes.ReadOnly = True
        Me.colHighTimes.Width = 60
        '
        'colIntensity
        '
        Me.colIntensity.HeaderText = "Intensity"
        Me.colIntensity.Name = "colIntensity"
        Me.colIntensity.ReadOnly = True
        Me.colIntensity.Width = 80
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(756, 649)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.graphInt)
        Me.Controls.Add(Me.groupBox4)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Sweeping - (c) Feasa Enterprises Ltd"
        Me.groupBox4.ResumeLayout(False)
        Me.groupBox4.PerformLayout()
        CType(Me.numFibers, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numCaptureTime, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numSampleCount, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numWaitTime, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.numStartDelay, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        CType(Me.datagridTimes, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

    Private WithEvents groupBox4 As GroupBox
    Private WithEvents numFibers As NumericUpDown
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
    Private WithEvents chkIsOffToOnPattern As CheckBox
    Friend WithEvents label3 As Label
    Friend WithEvents Label1 As Label
    Friend WithEvents lstPorts As ComboBox
    Friend WithEvents GroupBox1 As GroupBox
    Private WithEvents graphInt As ZedGraph.ZedGraphControl
    Friend WithEvents GroupBox2 As GroupBox
    Friend WithEvents datagridTimes As DataGridView
    Friend WithEvents Label2 As Label
    Friend WithEvents colFib As DataGridViewTextBoxColumn
    Friend WithEvents colLowTimes As DataGridViewTextBoxColumn
    Friend WithEvents colHighTimes As DataGridViewTextBoxColumn
    Friend WithEvents colIntensity As DataGridViewTextBoxColumn
End Class
