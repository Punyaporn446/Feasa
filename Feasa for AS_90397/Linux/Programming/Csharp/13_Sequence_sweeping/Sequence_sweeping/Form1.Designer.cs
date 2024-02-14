namespace Sequence_sweeping
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
            this.components = new System.ComponentModel.Container();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.graphInt = new ZedGraph.ZedGraphControl();
            this.Label2 = new System.Windows.Forms.Label();
            this.numFibers = new System.Windows.Forms.NumericUpDown();
            this.btnSequenceTest = new System.Windows.Forms.Button();
            this.numCaptureTime = new System.Windows.Forms.NumericUpDown();
            this.label12 = new System.Windows.Forms.Label();
            this.chkIsOffToOnPattern = new System.Windows.Forms.CheckBox();
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.datagridTimes = new System.Windows.Forms.DataGridView();
            this.colFib = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colLowTimes = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colHighTimes = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.colIntensity = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.numSampleCount = new System.Windows.Forms.NumericUpDown();
            this.numWaitTime = new System.Windows.Forms.NumericUpDown();
            this.label10 = new System.Windows.Forms.Label();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.numStartDelay = new System.Windows.Forms.NumericUpDown();
            this.label9 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            ((System.ComponentModel.ISupportInitialize)(this.numFibers)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numCaptureTime)).BeginInit();
            this.GroupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.datagridTimes)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numSampleCount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numWaitTime)).BeginInit();
            this.GroupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numStartDelay)).BeginInit();
            this.groupBox4.SuspendLayout();
            this.SuspendLayout();
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(77, 24);
            this.lstPorts.Name = "lstPorts";
            this.lstPorts.Size = new System.Drawing.Size(110, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // graphInt
            // 
            this.graphInt.IsEnableHPan = false;
            this.graphInt.IsEnableHZoom = false;
            this.graphInt.IsEnableVPan = false;
            this.graphInt.IsEnableVZoom = false;
            this.graphInt.IsEnableWheelZoom = false;
            this.graphInt.Location = new System.Drawing.Point(11, 277);
            this.graphInt.Name = "graphInt";
            this.graphInt.PanButtons = System.Windows.Forms.MouseButtons.None;
            this.graphInt.PanButtons2 = System.Windows.Forms.MouseButtons.None;
            this.graphInt.PanModifierKeys = System.Windows.Forms.Keys.None;
            this.graphInt.ScrollGrace = 0D;
            this.graphInt.ScrollMaxX = 0D;
            this.graphInt.ScrollMaxY = 0D;
            this.graphInt.ScrollMaxY2 = 0D;
            this.graphInt.ScrollMinX = 0D;
            this.graphInt.ScrollMinY = 0D;
            this.graphInt.ScrollMinY2 = 0D;
            this.graphInt.Size = new System.Drawing.Size(725, 361);
            this.graphInt.TabIndex = 35;
            // 
            // Label2
            // 
            this.Label2.AutoSize = true;
            this.Label2.Location = new System.Drawing.Point(173, 35);
            this.Label2.Name = "Label2";
            this.Label2.Size = new System.Drawing.Size(60, 13);
            this.Label2.TabIndex = 26;
            this.Label2.Text = "(test 1 to n)";
            this.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // numFibers
            // 
            this.numFibers.Location = new System.Drawing.Point(103, 33);
            this.numFibers.Maximum = new decimal(new int[] {
            20,
            0,
            0,
            0});
            this.numFibers.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numFibers.Name = "numFibers";
            this.numFibers.Size = new System.Drawing.Size(64, 20);
            this.numFibers.TabIndex = 23;
            this.numFibers.Value = new decimal(new int[] {
            12,
            0,
            0,
            0});
            // 
            // btnSequenceTest
            // 
            this.btnSequenceTest.Location = new System.Drawing.Point(16, 189);
            this.btnSequenceTest.Name = "btnSequenceTest";
            this.btnSequenceTest.Size = new System.Drawing.Size(212, 43);
            this.btnSequenceTest.TabIndex = 25;
            this.btnSequenceTest.Text = "SEQUENCE TEST";
            this.btnSequenceTest.UseVisualStyleBackColor = true;
            this.btnSequenceTest.Click += new System.EventHandler(this.btnSequenceTest_Click);
            // 
            // numCaptureTime
            // 
            this.numCaptureTime.Location = new System.Drawing.Point(103, 85);
            this.numCaptureTime.Maximum = new decimal(new int[] {
            999,
            0,
            0,
            0});
            this.numCaptureTime.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numCaptureTime.Name = "numCaptureTime";
            this.numCaptureTime.Size = new System.Drawing.Size(64, 20);
            this.numCaptureTime.TabIndex = 15;
            this.numCaptureTime.Value = new decimal(new int[] {
            4,
            0,
            0,
            0});
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(30, 35);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(70, 13);
            this.label12.TabIndex = 24;
            this.label12.Text = "Fibers to test:";
            this.label12.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // chkIsOffToOnPattern
            // 
            this.chkIsOffToOnPattern.AutoSize = true;
            this.chkIsOffToOnPattern.Checked = true;
            this.chkIsOffToOnPattern.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkIsOffToOnPattern.Location = new System.Drawing.Point(51, 163);
            this.chkIsOffToOnPattern.Name = "chkIsOffToOnPattern";
            this.chkIsOffToOnPattern.Size = new System.Drawing.Size(116, 17);
            this.chkIsOffToOnPattern.TabIndex = 10;
            this.chkIsOffToOnPattern.Text = "Is Off-to-On pattern";
            this.chkIsOffToOnPattern.UseVisualStyleBackColor = true;
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.datagridTimes);
            this.GroupBox2.Location = new System.Drawing.Point(474, 13);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(271, 258);
            this.GroupBox2.TabIndex = 36;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Timing";
            // 
            // datagridTimes
            // 
            this.datagridTimes.AllowUserToAddRows = false;
            this.datagridTimes.AllowUserToDeleteRows = false;
            this.datagridTimes.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.datagridTimes.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.colFib,
            this.colLowTimes,
            this.colHighTimes,
            this.colIntensity});
            this.datagridTimes.Location = new System.Drawing.Point(9, 18);
            this.datagridTimes.Name = "datagridTimes";
            this.datagridTimes.ReadOnly = true;
            this.datagridTimes.RowHeadersVisible = false;
            this.datagridTimes.Size = new System.Drawing.Size(252, 226);
            this.datagridTimes.TabIndex = 0;
            // 
            // colFib
            // 
            this.colFib.HeaderText = "Fib";
            this.colFib.Name = "colFib";
            this.colFib.ReadOnly = true;
            this.colFib.Width = 35;
            // 
            // colLowTimes
            // 
            this.colLowTimes.HeaderText = "Low (ms)";
            this.colLowTimes.Name = "colLowTimes";
            this.colLowTimes.ReadOnly = true;
            this.colLowTimes.Width = 60;
            // 
            // colHighTimes
            // 
            this.colHighTimes.HeaderText = "High (ms)";
            this.colHighTimes.Name = "colHighTimes";
            this.colHighTimes.ReadOnly = true;
            this.colHighTimes.Width = 60;
            // 
            // colIntensity
            // 
            this.colIntensity.HeaderText = "Intensity";
            this.colIntensity.Name = "colIntensity";
            this.colIntensity.ReadOnly = true;
            this.colIntensity.Width = 80;
            // 
            // numSampleCount
            // 
            this.numSampleCount.Increment = new decimal(new int[] {
            25,
            0,
            0,
            0});
            this.numSampleCount.Location = new System.Drawing.Point(103, 137);
            this.numSampleCount.Maximum = new decimal(new int[] {
            3500,
            0,
            0,
            0});
            this.numSampleCount.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numSampleCount.Name = "numSampleCount";
            this.numSampleCount.Size = new System.Drawing.Size(64, 20);
            this.numSampleCount.TabIndex = 16;
            this.numSampleCount.Value = new decimal(new int[] {
            200,
            0,
            0,
            0});
            // 
            // numWaitTime
            // 
            this.numWaitTime.Location = new System.Drawing.Point(103, 111);
            this.numWaitTime.Maximum = new decimal(new int[] {
            999,
            0,
            0,
            0});
            this.numWaitTime.Name = "numWaitTime";
            this.numWaitTime.Size = new System.Drawing.Size(64, 20);
            this.numWaitTime.TabIndex = 17;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(20, 87);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(69, 13);
            this.label10.TabIndex = 18;
            this.label10.Text = "Capture time:";
            this.label10.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.label3);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(11, 13);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(209, 258);
            this.GroupBox1.TabIndex = 33;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications setup";
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(15, 27);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(56, 13);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "COM Port:";
            // 
            // label3
            // 
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(18, 61);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(169, 184);
            this.label3.TabIndex = 20;
            this.label3.Text = "This example explores the Sequence functionality applied to the more and more com" +
    "mon automotive sweeping indicators";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(30, 61);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(60, 13);
            this.label11.TabIndex = 22;
            this.label11.Text = "Start delay:";
            this.label11.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // numStartDelay
            // 
            this.numStartDelay.Location = new System.Drawing.Point(103, 59);
            this.numStartDelay.Maximum = new decimal(new int[] {
            999,
            0,
            0,
            0});
            this.numStartDelay.Name = "numStartDelay";
            this.numStartDelay.Size = new System.Drawing.Size(64, 20);
            this.numStartDelay.TabIndex = 21;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(41, 113);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(54, 13);
            this.label9.TabIndex = 19;
            this.label9.Text = "Wait time:";
            this.label9.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(20, 139);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(75, 13);
            this.label8.TabIndex = 20;
            this.label8.Text = "Sample count:";
            this.label8.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.Label2);
            this.groupBox4.Controls.Add(this.numFibers);
            this.groupBox4.Controls.Add(this.btnSequenceTest);
            this.groupBox4.Controls.Add(this.numCaptureTime);
            this.groupBox4.Controls.Add(this.label12);
            this.groupBox4.Controls.Add(this.chkIsOffToOnPattern);
            this.groupBox4.Controls.Add(this.numSampleCount);
            this.groupBox4.Controls.Add(this.numWaitTime);
            this.groupBox4.Controls.Add(this.label11);
            this.groupBox4.Controls.Add(this.label10);
            this.groupBox4.Controls.Add(this.numStartDelay);
            this.groupBox4.Controls.Add(this.label9);
            this.groupBox4.Controls.Add(this.label8);
            this.groupBox4.Location = new System.Drawing.Point(226, 13);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(242, 258);
            this.groupBox4.TabIndex = 34;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Sequence Test";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(756, 650);
            this.Controls.Add(this.graphInt);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Controls.Add(this.groupBox4);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Sequence - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            ((System.ComponentModel.ISupportInitialize)(this.numFibers)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numCaptureTime)).EndInit();
            this.GroupBox2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.datagridTimes)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numSampleCount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numWaitTime)).EndInit();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numStartDelay)).EndInit();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.ComboBox lstPorts;
        private ZedGraph.ZedGraphControl graphInt;
        internal System.Windows.Forms.Label Label2;
        private System.Windows.Forms.NumericUpDown numFibers;
        internal System.Windows.Forms.Button btnSequenceTest;
        private System.Windows.Forms.NumericUpDown numCaptureTime;
        internal System.Windows.Forms.Label label12;
        private System.Windows.Forms.CheckBox chkIsOffToOnPattern;
        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.DataGridView datagridTimes;
        internal System.Windows.Forms.DataGridViewTextBoxColumn colFib;
        internal System.Windows.Forms.DataGridViewTextBoxColumn colLowTimes;
        internal System.Windows.Forms.DataGridViewTextBoxColumn colHighTimes;
        internal System.Windows.Forms.DataGridViewTextBoxColumn colIntensity;
        private System.Windows.Forms.NumericUpDown numSampleCount;
        private System.Windows.Forms.NumericUpDown numWaitTime;
        internal System.Windows.Forms.Label label10;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.Label label3;
        internal System.Windows.Forms.Label label11;
        private System.Windows.Forms.NumericUpDown numStartDelay;
        internal System.Windows.Forms.Label label9;
        internal System.Windows.Forms.Label label8;
        private System.Windows.Forms.GroupBox groupBox4;
    }
}

