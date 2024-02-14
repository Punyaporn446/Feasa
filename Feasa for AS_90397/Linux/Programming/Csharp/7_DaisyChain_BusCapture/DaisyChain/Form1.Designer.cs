namespace DaisyChain
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
            this.GroupBox4 = new System.Windows.Forms.GroupBox();
            this.Label6 = new System.Windows.Forms.Label();
            this.cmdRead = new System.Windows.Forms.Button();
            this.GroupBox3 = new System.Windows.Forms.GroupBox();
            this.txtLog = new System.Windows.Forms.TextBox();
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.Label3 = new System.Windows.Forms.Label();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.lstCaptureRange = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.lstCaptureMode = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.btnAddAnalyser = new System.Windows.Forms.Button();
            this.lstAnalysers = new System.Windows.Forms.ListBox();
            this.txtSN = new System.Windows.Forms.TextBox();
            this.numFiber = new System.Windows.Forms.NumericUpDown();
            this.GroupBox4.SuspendLayout();
            this.GroupBox3.SuspendLayout();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numFiber)).BeginInit();
            this.SuspendLayout();
            // 
            // GroupBox4
            // 
            this.GroupBox4.Controls.Add(this.numFiber);
            this.GroupBox4.Controls.Add(this.lstCaptureRange);
            this.GroupBox4.Controls.Add(this.label2);
            this.GroupBox4.Controls.Add(this.lstCaptureMode);
            this.GroupBox4.Controls.Add(this.label5);
            this.GroupBox4.Controls.Add(this.Label6);
            this.GroupBox4.Controls.Add(this.cmdRead);
            this.GroupBox4.Location = new System.Drawing.Point(204, 12);
            this.GroupBox4.Name = "GroupBox4";
            this.GroupBox4.Size = new System.Drawing.Size(245, 158);
            this.GroupBox4.TabIndex = 10;
            this.GroupBox4.TabStop = false;
            this.GroupBox4.Text = "Measure";
            // 
            // Label6
            // 
            this.Label6.AutoSize = true;
            this.Label6.Location = new System.Drawing.Point(104, 98);
            this.Label6.Name = "Label6";
            this.Label6.Size = new System.Drawing.Size(74, 13);
            this.Label6.TabIndex = 11;
            this.Label6.Text = "Fiber to Read:";
            // 
            // cmdRead
            // 
            this.cmdRead.Location = new System.Drawing.Point(6, 122);
            this.cmdRead.Name = "cmdRead";
            this.cmdRead.Size = new System.Drawing.Size(229, 27);
            this.cmdRead.TabIndex = 10;
            this.cmdRead.Text = "BUS CAPTURE + READ";
            this.cmdRead.UseVisualStyleBackColor = true;
            this.cmdRead.Click += new System.EventHandler(this.cmdRead_Click);
            // 
            // GroupBox3
            // 
            this.GroupBox3.Controls.Add(this.txtLog);
            this.GroupBox3.Location = new System.Drawing.Point(204, 176);
            this.GroupBox3.Name = "GroupBox3";
            this.GroupBox3.Size = new System.Drawing.Size(245, 128);
            this.GroupBox3.TabIndex = 9;
            this.GroupBox3.TabStop = false;
            this.GroupBox3.Text = "Response";
            // 
            // txtLog
            // 
            this.txtLog.Location = new System.Drawing.Point(9, 19);
            this.txtLog.Multiline = true;
			this.txtLog.Name = "txtLog";
			this.txtLog.BackColor = System.Drawing.Color.White;
            this.txtLog.Size = new System.Drawing.Size(226, 102);
            this.txtLog.TabIndex = 8;
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.txtSN);
            this.GroupBox2.Controls.Add(this.btnAddAnalyser);
            this.GroupBox2.Controls.Add(this.lstAnalysers);
            this.GroupBox2.Location = new System.Drawing.Point(17, 176);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(180, 128);
            this.GroupBox2.TabIndex = 8;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Daisy Chain";
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.Label3);
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(14, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(183, 158);
            this.GroupBox1.TabIndex = 7;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications Setup";
            // 
            // Label3
            // 
            this.Label3.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.Label3.Location = new System.Drawing.Point(6, 64);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(171, 85);
            this.Label3.TabIndex = 5;
            this.Label3.Text = "Select the port of the 1st Analyser (Bus Master) and then add all the devices att" +
                "ached to the Bus in the list below.";
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(6, 21);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(167, 13);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "Port of the 1st Led Analyser:";
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(89, 37);
			this.lstPorts.Name = "lstPorts";
			this.lstPorts.BackColor = System.Drawing.Color.White;
            this.lstPorts.Size = new System.Drawing.Size(84, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // lstCaptureRange
            // 
            this.lstCaptureRange.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstCaptureRange.FormattingEnabled = true;
            this.lstCaptureRange.Location = new System.Drawing.Point(171, 67);
			this.lstCaptureRange.Name = "lstCaptureRange";
			this.lstCaptureRange.BackColor = System.Drawing.Color.White;
            this.lstCaptureRange.Size = new System.Drawing.Size(64, 21);
            this.lstCaptureRange.TabIndex = 25;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(77, 70);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(85, 13);
            this.label2.TabIndex = 24;
            this.label2.Text = "Range (manual):";
            // 
            // lstCaptureMode
            // 
            this.lstCaptureMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstCaptureMode.FormattingEnabled = true;
            this.lstCaptureMode.Location = new System.Drawing.Point(8, 37);
			this.lstCaptureMode.Name = "lstCaptureMode";
			this.lstCaptureMode.BackColor = System.Drawing.Color.White;
            this.lstCaptureMode.Size = new System.Drawing.Size(227, 21);
            this.lstCaptureMode.TabIndex = 23;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(5, 21);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(77, 13);
            this.label5.TabIndex = 22;
            this.label5.Text = "Capture Mode:";
            // 
            // btnAddAnalyser
            // 
            this.btnAddAnalyser.Location = new System.Drawing.Point(6, 19);
            this.btnAddAnalyser.Name = "btnAddAnalyser";
            this.btnAddAnalyser.Size = new System.Drawing.Size(109, 27);
            this.btnAddAnalyser.TabIndex = 11;
            this.btnAddAnalyser.Text = "ADD Analyser:";
            this.btnAddAnalyser.UseVisualStyleBackColor = true;
            this.btnAddAnalyser.Click += new System.EventHandler(this.btnAddAnalyser_Click);
            // 
            // lstAnalysers
            // 
            this.lstAnalysers.FormattingEnabled = true;
            this.lstAnalysers.Location = new System.Drawing.Point(6, 52);
            this.lstAnalysers.Name = "lstAnalysers";
            this.lstAnalysers.Size = new System.Drawing.Size(164, 69);
			this.lstAnalysers.BackColor = System.Drawing.Color.White;
            this.lstAnalysers.TabIndex = 12;
            // 
            // txtSN
            // 
            this.txtSN.Location = new System.Drawing.Point(121, 23);
            this.txtSN.MaxLength = 4;
            this.txtSN.Name = "txtSN";
			this.txtSN.Size = new System.Drawing.Size(49, 20);
			this.txtSN.BackColor = System.Drawing.Color.White;
            this.txtSN.TabIndex = 13;
            this.txtSN.Text = "E218";
            // 
            // numFiber
            // 
            this.numFiber.Location = new System.Drawing.Point(184, 96);
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
			this.numFiber.BackColor = System.Drawing.Color.White;
            this.numFiber.Size = new System.Drawing.Size(50, 20);
            this.numFiber.TabIndex = 26;
            this.numFiber.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(458, 317);
            this.Controls.Add(this.GroupBox4);
            this.Controls.Add(this.GroupBox3);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Daisy Chain Bus Capture - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox4.ResumeLayout(false);
            this.GroupBox4.PerformLayout();
            this.GroupBox3.ResumeLayout(false);
            this.GroupBox3.PerformLayout();
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numFiber)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.GroupBox GroupBox4;
        internal System.Windows.Forms.Label Label6;
        internal System.Windows.Forms.Button cmdRead;
        internal System.Windows.Forms.GroupBox GroupBox3;
        internal System.Windows.Forms.TextBox txtLog;
        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label3;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
        internal System.Windows.Forms.ComboBox lstCaptureRange;
        internal System.Windows.Forms.Label label2;
        internal System.Windows.Forms.ComboBox lstCaptureMode;
        internal System.Windows.Forms.Label label5;
        private System.Windows.Forms.Button btnAddAnalyser;
        private System.Windows.Forms.ListBox lstAnalysers;
        internal System.Windows.Forms.TextBox txtSN;
        private System.Windows.Forms.NumericUpDown numFiber;
    }
}

