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
        Me.lstCaptureFrames = New System.Windows.Forms.ComboBox
        Me.Label4 = New System.Windows.Forms.Label
        Me.lstCaptureRange = New System.Windows.Forms.ComboBox
        Me.Label3 = New System.Windows.Forms.Label
        Me.lstCaptureMode = New System.Windows.Forms.ComboBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.cmdCapture = New System.Windows.Forms.Button
        Me.lstNumFibs = New System.Windows.Forms.ComboBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.dataGrid = New System.Windows.Forms.DataGridView
        Me.Red = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Green = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Blue = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Intensity = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.cmdCaptureEasy = New System.Windows.Forms.Button
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.dataGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(21, 27)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(56, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "COM Port:"
        '
        'cmdRead
        '
        Me.cmdRead.Location = New System.Drawing.Point(9, 266)
        Me.cmdRead.Name = "cmdRead"
        Me.cmdRead.Size = New System.Drawing.Size(207, 27)
        Me.cmdRead.TabIndex = 1
        Me.cmdRead.Text = "READ DATA"
        Me.cmdRead.UseVisualStyleBackColor = True
        '
        'lstPorts
        '
        Me.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstPorts.FormattingEnabled = True
        Me.lstPorts.Location = New System.Drawing.Point(83, 24)
        Me.lstPorts.Name = "lstPorts"
        Me.lstPorts.Size = New System.Drawing.Size(64, 21)
        Me.lstPorts.TabIndex = 2
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(274, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(222, 58)
        Me.GroupBox1.TabIndex = 3
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications Setup"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.cmdCaptureEasy)
        Me.GroupBox2.Controls.Add(Me.lstCaptureFrames)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Controls.Add(Me.lstCaptureRange)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.lstCaptureMode)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.cmdCapture)
        Me.GroupBox2.Controls.Add(Me.lstNumFibs)
        Me.GroupBox2.Controls.Add(Me.Label6)
        Me.GroupBox2.Controls.Add(Me.cmdRead)
        Me.GroupBox2.Location = New System.Drawing.Point(274, 76)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(222, 319)
        Me.GroupBox2.TabIndex = 4
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Commands"
        '
        'lstCaptureFrames
        '
        Me.lstCaptureFrames.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstCaptureFrames.FormattingEnabled = True
        Me.lstCaptureFrames.Location = New System.Drawing.Point(162, 92)
        Me.lstCaptureFrames.Name = "lstCaptureFrames"
        Me.lstCaptureFrames.Size = New System.Drawing.Size(54, 21)
        Me.lstCaptureFrames.TabIndex = 23
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(6, 95)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(80, 13)
        Me.Label4.TabIndex = 22
        Me.Label4.Text = "Frames (PWM):"
        '
        'lstCaptureRange
        '
        Me.lstCaptureRange.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstCaptureRange.FormattingEnabled = True
        Me.lstCaptureRange.Location = New System.Drawing.Point(124, 65)
        Me.lstCaptureRange.Name = "lstCaptureRange"
        Me.lstCaptureRange.Size = New System.Drawing.Size(92, 21)
        Me.lstCaptureRange.TabIndex = 21
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(5, 68)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(117, 13)
        Me.Label3.TabIndex = 20
        Me.Label3.Text = "Range (manual/PWM):"
        '
        'lstCaptureMode
        '
        Me.lstCaptureMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstCaptureMode.FormattingEnabled = True
        Me.lstCaptureMode.Location = New System.Drawing.Point(9, 38)
        Me.lstCaptureMode.Name = "lstCaptureMode"
        Me.lstCaptureMode.Size = New System.Drawing.Size(207, 21)
        Me.lstCaptureMode.TabIndex = 19
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(6, 22)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(37, 13)
        Me.Label2.TabIndex = 18
        Me.Label2.Text = "Mode:"
        '
        'cmdCapture
        '
        Me.cmdCapture.Location = New System.Drawing.Point(8, 133)
        Me.cmdCapture.Name = "cmdCapture"
        Me.cmdCapture.Size = New System.Drawing.Size(208, 29)
        Me.cmdCapture.TabIndex = 17
        Me.cmdCapture.Text = "CAPTURE"
        Me.cmdCapture.UseVisualStyleBackColor = True
        '
        'lstNumFibs
        '
        Me.lstNumFibs.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.lstNumFibs.FormattingEnabled = True
        Me.lstNumFibs.Location = New System.Drawing.Point(83, 231)
        Me.lstNumFibs.Name = "lstNumFibs"
        Me.lstNumFibs.Size = New System.Drawing.Size(64, 21)
        Me.lstNumFibs.TabIndex = 16
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(6, 234)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(79, 13)
        Me.Label6.TabIndex = 8
        Me.Label6.Text = "Fibers to Read:"
        '
        'dataGrid
        '
        Me.dataGrid.AllowUserToAddRows = False
        Me.dataGrid.AllowUserToDeleteRows = False
        Me.dataGrid.AllowUserToResizeColumns = False
        Me.dataGrid.AllowUserToResizeRows = False
        Me.dataGrid.BackgroundColor = System.Drawing.SystemColors.Control
        Me.dataGrid.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.dataGrid.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.Disable
        Me.dataGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dataGrid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Red, Me.Green, Me.Blue, Me.Intensity})
        Me.dataGrid.EnableHeadersVisualStyles = False
        Me.dataGrid.GridColor = System.Drawing.SystemColors.ButtonFace
        Me.dataGrid.Location = New System.Drawing.Point(12, 12)
        Me.dataGrid.MultiSelect = False
        Me.dataGrid.Name = "dataGrid"
        Me.dataGrid.ReadOnly = True
        Me.dataGrid.RowHeadersVisible = False
        Me.dataGrid.RowTemplate.Height = 18
        Me.dataGrid.Size = New System.Drawing.Size(256, 383)
        Me.dataGrid.TabIndex = 10
        '
        'Red
        '
        Me.Red.HeaderText = "Red"
        Me.Red.Name = "Red"
        Me.Red.ReadOnly = True
        Me.Red.Width = 50
        '
        'Green
        '
        Me.Green.HeaderText = "Green"
        Me.Green.Name = "Green"
        Me.Green.ReadOnly = True
        Me.Green.Width = 50
        '
        'Blue
        '
        Me.Blue.HeaderText = "Blue"
        Me.Blue.Name = "Blue"
        Me.Blue.ReadOnly = True
        Me.Blue.Width = 50
        '
        'Intensity
        '
        Me.Intensity.HeaderText = "Intensity"
        Me.Intensity.Name = "Intensity"
        Me.Intensity.ReadOnly = True
        '
        'cmdCaptureEasy
        '
        Me.cmdCaptureEasy.Location = New System.Drawing.Point(8, 168)
        Me.cmdCaptureEasy.Name = "cmdCaptureEasy"
        Me.cmdCaptureEasy.Size = New System.Drawing.Size(208, 29)
        Me.cmdCaptureEasy.TabIndex = 24
        Me.cmdCaptureEasy.Text = "CAPTURE (easy method)"
        Me.cmdCaptureEasy.UseVisualStyleBackColor = True
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(508, 407)
        Me.Controls.Add(Me.dataGrid)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Capture Modes - (c) Feasa Enterprises Ltd"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.dataGrid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents cmdRead As System.Windows.Forms.Button
    Friend WithEvents lstPorts As System.Windows.Forms.ComboBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents lstNumFibs As System.Windows.Forms.ComboBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents cmdCapture As System.Windows.Forms.Button
    Friend WithEvents dataGrid As System.Windows.Forms.DataGridView
    Friend WithEvents Red As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Green As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Blue As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Intensity As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents lstCaptureFrames As System.Windows.Forms.ComboBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents lstCaptureRange As System.Windows.Forms.ComboBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents lstCaptureMode As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents cmdCaptureEasy As System.Windows.Forms.Button

End Class
