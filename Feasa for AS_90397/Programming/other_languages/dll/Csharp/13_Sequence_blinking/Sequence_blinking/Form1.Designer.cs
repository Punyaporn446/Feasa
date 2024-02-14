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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.btnFindParams = new System.Windows.Forms.Button();
            this.label7 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.chkTimeResImportant = new System.Windows.Forms.CheckBox();
            this.numLEDCount = new System.Windows.Forms.NumericUpDown();
            this.numFiberToTest = new System.Windows.Forms.NumericUpDown();
            this.numCycles = new System.Windows.Forms.NumericUpDown();
            this.label4 = new System.Windows.Forms.Label();
            this.lstSignalSpeed = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.lstBlinkingSpeed = new System.Windows.Forms.ComboBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.numWaitTime = new System.Windows.Forms.NumericUpDown();
            this.numSampleCount = new System.Windows.Forms.NumericUpDown();
            this.numCaptureTime = new System.Windows.Forms.NumericUpDown();
            this.label11 = new System.Windows.Forms.Label();
            this.numStartDelay = new System.Windows.Forms.NumericUpDown();
            this.label12 = new System.Windows.Forms.Label();
            this.numFiber = new System.Windows.Forms.NumericUpDown();
            this.btnSequenceTest = new System.Windows.Forms.Button();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.graphInt = new ZedGraph.ZedGraphControl();
            this.graphCIE = new ZedGraph.ZedGraphControl();
            this.GroupBox1.SuspendLayout();
            this.groupBox3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numLEDCount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numFiberToTest)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numCycles)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numWaitTime)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numSampleCount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numCaptureTime)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numStartDelay)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numFiber)).BeginInit();
            this.groupBox4.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.label3);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(12, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(271, 258);
            this.GroupBox1.TabIndex = 11;
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
            this.label3.Size = new System.Drawing.Size(232, 184);
            this.label3.TabIndex = 20;
            this.label3.Text = resources.GetString("label3.Text");
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(77, 24);
            this.lstPorts.Name = "lstPorts";
            this.lstPorts.Size = new System.Drawing.Size(123, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.btnFindParams);
            this.groupBox3.Controls.Add(this.label7);
            this.groupBox3.Controls.Add(this.label6);
            this.groupBox3.Controls.Add(this.label5);
            this.groupBox3.Controls.Add(this.chkTimeResImportant);
            this.groupBox3.Controls.Add(this.numLEDCount);
            this.groupBox3.Controls.Add(this.numFiberToTest);
            this.groupBox3.Controls.Add(this.numCycles);
            this.groupBox3.Controls.Add(this.label4);
            this.groupBox3.Controls.Add(this.lstSignalSpeed);
            this.groupBox3.Controls.Add(this.label2);
            this.groupBox3.Controls.Add(this.lstBlinkingSpeed);
            this.groupBox3.Location = new System.Drawing.Point(289, 12);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(262, 258);
            this.groupBox3.TabIndex = 14;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Find parameters";
            // 
            // btnFindParams
            // 
            this.btnFindParams.Location = new System.Drawing.Point(18, 189);
            this.btnFindParams.Name = "btnFindParams";
            this.btnFindParams.Size = new System.Drawing.Size(233, 43);
            this.btnFindParams.TabIndex = 18;
            this.btnFindParams.Text = "FIND PARAMETERS";
            this.btnFindParams.UseVisualStyleBackColor = true;
            this.btnFindParams.Click += new System.EventHandler(this.btnFindParams_Click);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(114, 135);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(65, 13);
            this.label7.TabIndex = 13;
            this.label7.Text = "Fiber to test:";
            this.label7.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(90, 109);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(89, 13);
            this.label6.TabIndex = 12;
            this.label6.Text = "Total LED Count:";
            this.label6.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(68, 83);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(111, 13);
            this.label5.TabIndex = 11;
            this.label5.Text = "Min cycles to capture:";
            this.label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // chkTimeResImportant
            // 
            this.chkTimeResImportant.AutoSize = true;
            this.chkTimeResImportant.Checked = true;
            this.chkTimeResImportant.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkTimeResImportant.Location = new System.Drawing.Point(18, 166);
            this.chkTimeResImportant.Name = "chkTimeResImportant";
            this.chkTimeResImportant.Size = new System.Drawing.Size(153, 17);
            this.chkTimeResImportant.TabIndex = 10;
            this.chkTimeResImportant.Text = "Time resolution is important";
            this.chkTimeResImportant.UseVisualStyleBackColor = true;
            // 
            // numLEDCount
            // 
            this.numLEDCount.Location = new System.Drawing.Point(187, 107);
            this.numLEDCount.Maximum = new decimal(new int[] {
            20,
            0,
            0,
            0});
            this.numLEDCount.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numLEDCount.Name = "numLEDCount";
            this.numLEDCount.Size = new System.Drawing.Size(64, 20);
            this.numLEDCount.TabIndex = 9;
            this.numLEDCount.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // numFiberToTest
            // 
            this.numFiberToTest.Location = new System.Drawing.Point(187, 133);
            this.numFiberToTest.Maximum = new decimal(new int[] {
            20,
            0,
            0,
            0});
            this.numFiberToTest.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numFiberToTest.Name = "numFiberToTest";
            this.numFiberToTest.Size = new System.Drawing.Size(64, 20);
            this.numFiberToTest.TabIndex = 8;
            this.numFiberToTest.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // numCycles
            // 
            this.numCycles.Location = new System.Drawing.Point(187, 81);
            this.numCycles.Maximum = new decimal(new int[] {
            50,
            0,
            0,
            0});
            this.numCycles.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numCycles.Name = "numCycles";
            this.numCycles.Size = new System.Drawing.Size(64, 20);
            this.numCycles.TabIndex = 7;
            this.numCycles.Value = new decimal(new int[] {
            4,
            0,
            0,
            0});
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(15, 57);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(73, 13);
            this.label4.TabIndex = 5;
            this.label4.Text = "Signal Speed:";
            // 
            // lstSignalSpeed
            // 
            this.lstSignalSpeed.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstSignalSpeed.FormattingEnabled = true;
            this.lstSignalSpeed.Location = new System.Drawing.Point(102, 54);
            this.lstSignalSpeed.Name = "lstSignalSpeed";
            this.lstSignalSpeed.Size = new System.Drawing.Size(149, 21);
            this.lstSignalSpeed.TabIndex = 6;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(15, 30);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(81, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Blinking Speed:";
            // 
            // lstBlinkingSpeed
            // 
            this.lstBlinkingSpeed.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstBlinkingSpeed.FormattingEnabled = true;
            this.lstBlinkingSpeed.Location = new System.Drawing.Point(102, 27);
            this.lstBlinkingSpeed.Name = "lstBlinkingSpeed";
            this.lstBlinkingSpeed.Size = new System.Drawing.Size(149, 21);
            this.lstBlinkingSpeed.TabIndex = 4;
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
            // numSampleCount
            // 
            this.numSampleCount.Location = new System.Drawing.Point(103, 137);
            this.numSampleCount.Maximum = new decimal(new int[] {
            9999,
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
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(30, 35);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(65, 13);
            this.label12.TabIndex = 24;
            this.label12.Text = "Fiber to test:";
            this.label12.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // numFiber
            // 
            this.numFiber.Location = new System.Drawing.Point(103, 33);
            this.numFiber.Maximum = new decimal(new int[] {
            20,
            0,
            0,
            0});
            this.numFiber.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numFiber.Name = "numFiber";
            this.numFiber.Size = new System.Drawing.Size(64, 20);
            this.numFiber.TabIndex = 23;
            this.numFiber.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // btnSequenceTest
            // 
            this.btnSequenceTest.Location = new System.Drawing.Point(16, 189);
            this.btnSequenceTest.Name = "btnSequenceTest";
            this.btnSequenceTest.Size = new System.Drawing.Size(151, 43);
            this.btnSequenceTest.TabIndex = 25;
            this.btnSequenceTest.Text = "SEQUENCE TEST";
            this.btnSequenceTest.UseVisualStyleBackColor = true;
            this.btnSequenceTest.Click += new System.EventHandler(this.btnSequenceTest_Click);
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.numFiber);
            this.groupBox4.Controls.Add(this.btnSequenceTest);
            this.groupBox4.Controls.Add(this.numCaptureTime);
            this.groupBox4.Controls.Add(this.label12);
            this.groupBox4.Controls.Add(this.numSampleCount);
            this.groupBox4.Controls.Add(this.numWaitTime);
            this.groupBox4.Controls.Add(this.label11);
            this.groupBox4.Controls.Add(this.label10);
            this.groupBox4.Controls.Add(this.numStartDelay);
            this.groupBox4.Controls.Add(this.label9);
            this.groupBox4.Controls.Add(this.label8);
            this.groupBox4.Location = new System.Drawing.Point(557, 12);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(180, 258);
            this.groupBox4.TabIndex = 26;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Sequence Test";
            // 
            // graphInt
            // 
            this.graphInt.IsEnableHPan = false;
            this.graphInt.IsEnableHZoom = false;
            this.graphInt.IsEnableVPan = false;
            this.graphInt.IsEnableVZoom = false;
            this.graphInt.IsEnableWheelZoom = false;
            this.graphInt.Location = new System.Drawing.Point(12, 276);
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
            this.graphInt.Size = new System.Drawing.Size(725, 176);
            this.graphInt.TabIndex = 27;
            // 
            // graphCIE
            // 
            this.graphCIE.IsEnableHPan = false;
            this.graphCIE.IsEnableHZoom = false;
            this.graphCIE.IsEnableVPan = false;
            this.graphCIE.IsEnableVZoom = false;
            this.graphCIE.IsEnableWheelZoom = false;
            this.graphCIE.Location = new System.Drawing.Point(12, 458);
            this.graphCIE.Name = "graphCIE";
            this.graphCIE.PanButtons = System.Windows.Forms.MouseButtons.None;
            this.graphCIE.PanButtons2 = System.Windows.Forms.MouseButtons.None;
            this.graphCIE.PanModifierKeys = System.Windows.Forms.Keys.None;
            this.graphCIE.ScrollGrace = 0D;
            this.graphCIE.ScrollMaxX = 0D;
            this.graphCIE.ScrollMaxY = 0D;
            this.graphCIE.ScrollMaxY2 = 0D;
            this.graphCIE.ScrollMinX = 0D;
            this.graphCIE.ScrollMinY = 0D;
            this.graphCIE.ScrollMinY2 = 0D;
            this.graphCIE.Size = new System.Drawing.Size(725, 176);
            this.graphCIE.TabIndex = 28;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(756, 650);
            this.Controls.Add(this.graphCIE);
            this.Controls.Add(this.graphInt);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Sequence - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numLEDCount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numFiberToTest)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numCycles)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numWaitTime)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numSampleCount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numCaptureTime)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numStartDelay)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numFiber)).EndInit();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
        private System.Windows.Forms.GroupBox groupBox3;
        internal System.Windows.Forms.Label label3;
        internal System.Windows.Forms.Label label7;
        internal System.Windows.Forms.Label label6;
        internal System.Windows.Forms.Label label5;
        private System.Windows.Forms.CheckBox chkTimeResImportant;
        private System.Windows.Forms.NumericUpDown numLEDCount;
        private System.Windows.Forms.NumericUpDown numFiberToTest;
        private System.Windows.Forms.NumericUpDown numCycles;
        internal System.Windows.Forms.Label label4;
        internal System.Windows.Forms.ComboBox lstSignalSpeed;
        internal System.Windows.Forms.Label label2;
        internal System.Windows.Forms.ComboBox lstBlinkingSpeed;
        internal System.Windows.Forms.Button btnFindParams;
        internal System.Windows.Forms.Label label8;
        internal System.Windows.Forms.Label label9;
        internal System.Windows.Forms.Label label10;
        private System.Windows.Forms.NumericUpDown numWaitTime;
        private System.Windows.Forms.NumericUpDown numSampleCount;
        private System.Windows.Forms.NumericUpDown numCaptureTime;
        internal System.Windows.Forms.Label label11;
        private System.Windows.Forms.NumericUpDown numStartDelay;
        internal System.Windows.Forms.Label label12;
        private System.Windows.Forms.NumericUpDown numFiber;
        internal System.Windows.Forms.Button btnSequenceTest;
        private System.Windows.Forms.GroupBox groupBox4;
        private ZedGraph.ZedGraphControl graphInt;
        private ZedGraph.ZedGraphControl graphCIE;
    }
}

