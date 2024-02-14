namespace getRGBI
{
    partial class Form1
    {
        /// <summary>
        /// Variable del diseñador requerida.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Limpiar los recursos que se estén utilizando.
        /// </summary>
        /// <param name="disposing">true si los recursos administrados se deben eliminar; false en caso contrario, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Código generado por el Diseñador de Windows Forms

        /// <summary>
        /// Método necesario para admitir el Diseñador. No se puede modificar
        /// el contenido del método con el editor de código.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.dataGrid = new System.Windows.Forms.DataGridView();
            this.colSN = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colFiber = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colHue = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colSaturation = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colIntensity = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.lblExecutionTime = new System.Windows.Forms.Label();
            this.cmdCapture = new System.Windows.Forms.Button();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.label2 = new System.Windows.Forms.Label();
            this.btnAdd = new System.Windows.Forms.Button();
            this.lstPortsToTest = new System.Windows.Forms.ListBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.label3 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.dataGrid)).BeginInit();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
            // 
            // dataGrid
            // 
            this.dataGrid.AllowUserToAddRows = false;
            this.dataGrid.AllowUserToDeleteRows = false;
            this.dataGrid.AllowUserToResizeColumns = false;
            this.dataGrid.AllowUserToResizeRows = false;
            this.dataGrid.BackgroundColor = System.Drawing.SystemColors.Control;
            this.dataGrid.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.dataGrid.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.Disable;
            this.dataGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGrid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.colSN,
            this.colFiber,
            this.colHue,
            this.colSaturation,
            this.colIntensity});
            this.dataGrid.EnableHeadersVisualStyles = false;
            this.dataGrid.GridColor = System.Drawing.SystemColors.ButtonFace;
            this.dataGrid.Location = new System.Drawing.Point(11, 12);
            this.dataGrid.MultiSelect = false;
            this.dataGrid.Name = "dataGrid";
            this.dataGrid.ReadOnly = true;
            this.dataGrid.RowHeadersVisible = false;
            this.dataGrid.RowTemplate.Height = 18;
            this.dataGrid.Size = new System.Drawing.Size(305, 383);
            this.dataGrid.TabIndex = 13;
            // 
            // colSN
            // 
            this.colSN.HeaderText = "SN";
            this.colSN.Name = "colSN";
            this.colSN.ReadOnly = true;
            this.colSN.Width = 45;
            // 
            // colFiber
            // 
            this.colFiber.HeaderText = "Fiber";
            this.colFiber.Name = "colFiber";
            this.colFiber.ReadOnly = true;
            this.colFiber.Width = 35;
            // 
            // colHue
            // 
            this.colHue.HeaderText = "Hue";
            this.colHue.Name = "colHue";
            this.colHue.ReadOnly = true;
            this.colHue.Width = 60;
            // 
            // colSaturation
            // 
            this.colSaturation.HeaderText = "Saturation";
            this.colSaturation.Name = "colSaturation";
            this.colSaturation.ReadOnly = true;
            this.colSaturation.Width = 60;
            // 
            // colIntensity
            // 
            this.colIntensity.HeaderText = "Intensity";
            this.colIntensity.Name = "colIntensity";
            this.colIntensity.ReadOnly = true;
            this.colIntensity.Width = 80;
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.lblExecutionTime);
            this.GroupBox2.Controls.Add(this.cmdCapture);
            this.GroupBox2.Location = new System.Drawing.Point(322, 264);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(211, 131);
            this.GroupBox2.TabIndex = 12;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Commands";
            // 
            // lblExecutionTime
            // 
            this.lblExecutionTime.AutoSize = true;
            this.lblExecutionTime.Location = new System.Drawing.Point(15, 95);
            this.lblExecutionTime.Name = "lblExecutionTime";
            this.lblExecutionTime.Size = new System.Drawing.Size(85, 13);
            this.lblExecutionTime.TabIndex = 20;
            this.lblExecutionTime.Text = "Execution time: -";
            // 
            // cmdCapture
            // 
            this.cmdCapture.Location = new System.Drawing.Point(18, 21);
            this.cmdCapture.Name = "cmdCapture";
            this.cmdCapture.Size = new System.Drawing.Size(174, 53);
            this.cmdCapture.TabIndex = 17;
            this.cmdCapture.Text = "CAPTURE && TEST";
            this.cmdCapture.UseVisualStyleBackColor = true;
            this.cmdCapture.Click += new System.EventHandler(this.cmdCapture_Click);
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.label2);
            this.GroupBox1.Controls.Add(this.btnAdd);
            this.GroupBox1.Controls.Add(this.lstPortsToTest);
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(322, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(211, 246);
            this.GroupBox1.TabIndex = 11;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications setup";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(15, 78);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(93, 13);
            this.label2.TabIndex = 19;
            this.label2.Text = "Ports to be tested:";
            // 
            // btnAdd
            // 
            this.btnAdd.Location = new System.Drawing.Point(100, 43);
            this.btnAdd.Name = "btnAdd";
            this.btnAdd.Size = new System.Drawing.Size(92, 21);
            this.btnAdd.TabIndex = 18;
            this.btnAdd.Text = "> add to test";
            this.btnAdd.UseVisualStyleBackColor = true;
            this.btnAdd.Click += new System.EventHandler(this.btnAdd_Click);
            // 
            // lstPortsToTest
            // 
            this.lstPortsToTest.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(177)));
            this.lstPortsToTest.FormattingEnabled = true;
            this.lstPortsToTest.ItemHeight = 16;
            this.lstPortsToTest.Location = new System.Drawing.Point(18, 97);
            this.lstPortsToTest.Name = "lstPortsToTest";
            this.lstPortsToTest.Size = new System.Drawing.Size(174, 132);
            this.lstPortsToTest.TabIndex = 3;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(15, 27);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(56, 13);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "Port:";
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(18, 43);
            this.lstPorts.Name = "lstPorts";
            this.lstPorts.Size = new System.Drawing.Size(76, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.label3);
            this.groupBox3.Location = new System.Drawing.Point(539, 12);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(208, 383);
            this.groupBox3.TabIndex = 14;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Tips";
            // 
            // label3
            // 
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(6, 16);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(196, 361);
            this.label3.TabIndex = 20;
            this.label3.Text = resources.GetString("label3.Text");
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(759, 406);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.dataGrid);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "FastTestMT - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            ((System.ComponentModel.ISupportInitialize)(this.dataGrid)).EndInit();
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.DataGridView dataGrid;
        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.Button cmdCapture;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
        internal System.Windows.Forms.Label label2;
        internal System.Windows.Forms.Button btnAdd;
        private System.Windows.Forms.ListBox lstPortsToTest;
        private System.Windows.Forms.GroupBox groupBox3;
        internal System.Windows.Forms.Label label3;
        private System.Windows.Forms.DataGridViewTextBoxColumn colSN;
        private System.Windows.Forms.DataGridViewTextBoxColumn colFiber;
        private System.Windows.Forms.DataGridViewTextBoxColumn colHue;
        private System.Windows.Forms.DataGridViewTextBoxColumn colSaturation;
        private System.Windows.Forms.DataGridViewTextBoxColumn colIntensity;
        internal System.Windows.Forms.Label lblExecutionTime;
    }
}

