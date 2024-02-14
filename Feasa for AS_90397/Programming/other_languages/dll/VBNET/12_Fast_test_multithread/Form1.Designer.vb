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
        Me.dataGrid = New System.Windows.Forms.DataGridView()
        Me.colSN = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colFiber = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colHue = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colSaturation = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colIntensity = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.groupBox3 = New System.Windows.Forms.GroupBox()
        Me.label3 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.lblExecutionTime = New System.Windows.Forms.Label()
        Me.cmdCapture = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.label2 = New System.Windows.Forms.Label()
        Me.btnAdd = New System.Windows.Forms.Button()
        Me.lstPortsToTest = New System.Windows.Forms.ListBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.lstPorts = New System.Windows.Forms.ComboBox()
        CType(Me.dataGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.groupBox3.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
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
        Me.dataGrid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.colSN, Me.colFiber, Me.colHue, Me.colSaturation, Me.colIntensity})
        Me.dataGrid.EnableHeadersVisualStyles = False
        Me.dataGrid.GridColor = System.Drawing.SystemColors.ButtonFace
        Me.dataGrid.Location = New System.Drawing.Point(12, 12)
        Me.dataGrid.MultiSelect = False
        Me.dataGrid.Name = "dataGrid"
        Me.dataGrid.ReadOnly = True
        Me.dataGrid.RowHeadersVisible = False
        Me.dataGrid.RowTemplate.Height = 18
        Me.dataGrid.Size = New System.Drawing.Size(305, 383)
        Me.dataGrid.TabIndex = 10
        '
        'colSN
        '
        Me.colSN.HeaderText = "SN"
        Me.colSN.Name = "colSN"
        Me.colSN.ReadOnly = True
        Me.colSN.Width = 45
        '
        'colFiber
        '
        Me.colFiber.HeaderText = "Fiber"
        Me.colFiber.Name = "colFiber"
        Me.colFiber.ReadOnly = True
        Me.colFiber.Width = 35
        '
        'colHue
        '
        Me.colHue.HeaderText = "Hue"
        Me.colHue.Name = "colHue"
        Me.colHue.ReadOnly = True
        Me.colHue.Width = 60
        '
        'colSaturation
        '
        Me.colSaturation.HeaderText = "Saturation"
        Me.colSaturation.Name = "colSaturation"
        Me.colSaturation.ReadOnly = True
        Me.colSaturation.Width = 60
        '
        'colIntensity
        '
        Me.colIntensity.HeaderText = "Intensity"
        Me.colIntensity.Name = "colIntensity"
        Me.colIntensity.ReadOnly = True
        Me.colIntensity.Width = 80
        '
        'groupBox3
        '
        Me.groupBox3.Controls.Add(Me.label3)
        Me.groupBox3.Location = New System.Drawing.Point(540, 12)
        Me.groupBox3.Name = "groupBox3"
        Me.groupBox3.Size = New System.Drawing.Size(208, 383)
        Me.groupBox3.TabIndex = 15
        Me.groupBox3.TabStop = False
        Me.groupBox3.Text = "Tips"
        '
        'label3
        '
        Me.label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.label3.Location = New System.Drawing.Point(6, 16)
        Me.label3.Name = "label3"
        Me.label3.Size = New System.Drawing.Size(196, 361)
        Me.label3.TabIndex = 20
        Me.label3.Text = resources.GetString("label3.Text")
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.lblExecutionTime)
        Me.GroupBox2.Controls.Add(Me.cmdCapture)
        Me.GroupBox2.Location = New System.Drawing.Point(323, 264)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(211, 131)
        Me.GroupBox2.TabIndex = 17
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Commands"
        '
        'lblExecutionTime
        '
        Me.lblExecutionTime.AutoSize = True
        Me.lblExecutionTime.Location = New System.Drawing.Point(15, 95)
        Me.lblExecutionTime.Name = "lblExecutionTime"
        Me.lblExecutionTime.Size = New System.Drawing.Size(85, 13)
        Me.lblExecutionTime.TabIndex = 20
        Me.lblExecutionTime.Text = "Execution time: -"
        '
        'cmdCapture
        '
        Me.cmdCapture.Location = New System.Drawing.Point(18, 21)
        Me.cmdCapture.Name = "cmdCapture"
        Me.cmdCapture.Size = New System.Drawing.Size(174, 53)
        Me.cmdCapture.TabIndex = 17
        Me.cmdCapture.Text = "CAPTURE && TEST"
        Me.cmdCapture.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.label2)
        Me.GroupBox1.Controls.Add(Me.btnAdd)
        Me.GroupBox1.Controls.Add(Me.lstPortsToTest)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lstPorts)
        Me.GroupBox1.Location = New System.Drawing.Point(323, 12)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(211, 246)
        Me.GroupBox1.TabIndex = 16
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Communications setup"
        '
        'label2
        '
        Me.label2.AutoSize = True
        Me.label2.Location = New System.Drawing.Point(15, 78)
        Me.label2.Name = "label2"
        Me.label2.Size = New System.Drawing.Size(93, 13)
        Me.label2.TabIndex = 19
        Me.label2.Text = "Ports to be tested:"
        '
        'btnAdd
        '
        Me.btnAdd.Location = New System.Drawing.Point(100, 43)
        Me.btnAdd.Name = "btnAdd"
        Me.btnAdd.Size = New System.Drawing.Size(92, 21)
        Me.btnAdd.TabIndex = 18
        Me.btnAdd.Text = "> add to test"
        Me.btnAdd.UseVisualStyleBackColor = True
        '
        'lstPortsToTest
        '
        Me.lstPortsToTest.Font = New System.Drawing.Font("Courier New", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(177, Byte))
        Me.lstPortsToTest.FormattingEnabled = True
        Me.lstPortsToTest.ItemHeight = 16
        Me.lstPortsToTest.Location = New System.Drawing.Point(18, 97)
        Me.lstPortsToTest.Name = "lstPortsToTest"
        Me.lstPortsToTest.Size = New System.Drawing.Size(174, 132)
        Me.lstPortsToTest.TabIndex = 3
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
        Me.lstPorts.Location = New System.Drawing.Point(18, 43)
        Me.lstPorts.Name = "lstPorts"
        Me.lstPorts.Size = New System.Drawing.Size(76, 21)
        Me.lstPorts.TabIndex = 2
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(764, 407)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.groupBox3)
        Me.Controls.Add(Me.dataGrid)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "FastTestMT - (c) Feasa Enterprises Ltd"
        CType(Me.dataGrid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.groupBox3.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents dataGrid As System.Windows.Forms.DataGridView
    Friend WithEvents colSN As DataGridViewTextBoxColumn
    Friend WithEvents colFiber As DataGridViewTextBoxColumn
    Friend WithEvents colHue As DataGridViewTextBoxColumn
    Friend WithEvents colSaturation As DataGridViewTextBoxColumn
    Friend WithEvents colIntensity As DataGridViewTextBoxColumn
    Private WithEvents groupBox3 As GroupBox
    Friend WithEvents label3 As Label
    Friend WithEvents GroupBox2 As GroupBox
    Friend WithEvents lblExecutionTime As Label
    Friend WithEvents cmdCapture As Button
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents label2 As Label
    Friend WithEvents btnAdd As Button
    Private WithEvents lstPortsToTest As ListBox
    Friend WithEvents Label1 As Label
    Friend WithEvents lstPorts As ComboBox
End Class
