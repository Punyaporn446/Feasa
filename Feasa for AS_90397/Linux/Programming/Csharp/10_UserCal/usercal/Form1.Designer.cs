namespace capture_and_read
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
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.txtRefAbsIntB = new System.Windows.Forms.TextBox();
            this.txtRefyB = new System.Windows.Forms.TextBox();
            this.txtRefxB = new System.Windows.Forms.TextBox();
            this.label11 = new System.Windows.Forms.Label();
            this.txtRefAbsIntG = new System.Windows.Forms.TextBox();
            this.txtRefyG = new System.Windows.Forms.TextBox();
            this.txtRefxG = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.txtRefAbsIntR = new System.Windows.Forms.TextBox();
            this.txtRefyR = new System.Windows.Forms.TextBox();
            this.txtRefxR = new System.Windows.Forms.TextBox();
            this.label9 = new System.Windows.Forms.Label();
            this.btnAdjustRGB = new System.Windows.Forms.Button();
            this.btnReadParams = new System.Windows.Forms.Button();
            this.txtRefy = new System.Windows.Forms.TextBox();
            this.txtRefx = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.txtRefWavelength = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.txtRefAbsInt = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.btnAdjustxy = new System.Windows.Forms.Button();
            this.btnAdjustWavelength = new System.Windows.Forms.Button();
            this.btnAdjustAbsInt = new System.Windows.Forms.Button();
            this.btnBalanceInt = new System.Windows.Forms.Button();
            this.txtLog = new System.Windows.Forms.TextBox();
            this.Label5 = new System.Windows.Forms.Label();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.label8 = new System.Windows.Forms.Label();
            this.optFlash = new System.Windows.Forms.RadioButton();
            this.optRAM = new System.Windows.Forms.RadioButton();
            this.label2 = new System.Windows.Forms.Label();
            this.lstCapture = new System.Windows.Forms.ComboBox();
            this.chkPWM = new System.Windows.Forms.CheckBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.label3 = new System.Windows.Forms.Label();
            this.numFibers = new System.Windows.Forms.NumericUpDown();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numFibers)).BeginInit();
            this.SuspendLayout();
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.txtRefAbsIntB);
            this.GroupBox2.Controls.Add(this.txtRefyB);
            this.GroupBox2.Controls.Add(this.txtRefxB);
            this.GroupBox2.Controls.Add(this.label11);
            this.GroupBox2.Controls.Add(this.txtRefAbsIntG);
            this.GroupBox2.Controls.Add(this.txtRefyG);
            this.GroupBox2.Controls.Add(this.txtRefxG);
            this.GroupBox2.Controls.Add(this.label10);
            this.GroupBox2.Controls.Add(this.txtRefAbsIntR);
            this.GroupBox2.Controls.Add(this.txtRefyR);
            this.GroupBox2.Controls.Add(this.txtRefxR);
            this.GroupBox2.Controls.Add(this.label9);
            this.GroupBox2.Controls.Add(this.btnAdjustRGB);
            this.GroupBox2.Controls.Add(this.btnReadParams);
            this.GroupBox2.Controls.Add(this.txtRefy);
            this.GroupBox2.Controls.Add(this.txtRefx);
            this.GroupBox2.Controls.Add(this.label7);
            this.GroupBox2.Controls.Add(this.txtRefWavelength);
            this.GroupBox2.Controls.Add(this.label6);
            this.GroupBox2.Controls.Add(this.txtRefAbsInt);
            this.GroupBox2.Controls.Add(this.label4);
            this.GroupBox2.Controls.Add(this.btnAdjustxy);
            this.GroupBox2.Controls.Add(this.btnAdjustWavelength);
            this.GroupBox2.Controls.Add(this.btnAdjustAbsInt);
            this.GroupBox2.Controls.Add(this.btnBalanceInt);
            this.GroupBox2.Location = new System.Drawing.Point(201, 12);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(192, 444);
            this.GroupBox2.TabIndex = 6;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Commands";
            // 
            // txtRefAbsIntB
            // 
            this.txtRefAbsIntB.Location = new System.Drawing.Point(121, 334);
            this.txtRefAbsIntB.MaxLength = 6;
            this.txtRefAbsIntB.Name = "txtRefAbsIntB";
            this.txtRefAbsIntB.Size = new System.Drawing.Size(55, 20);
            this.txtRefAbsIntB.TabIndex = 41;
            this.txtRefAbsIntB.Text = "0.550639";
            // 
            // txtRefyB
            // 
            this.txtRefyB.Location = new System.Drawing.Point(77, 334);
            this.txtRefyB.MaxLength = 6;
            this.txtRefyB.Name = "txtRefyB";
            this.txtRefyB.Size = new System.Drawing.Size(41, 20);
            this.txtRefyB.TabIndex = 40;
            this.txtRefyB.Text = "0.0388";
            // 
            // txtRefxB
            // 
            this.txtRefxB.Location = new System.Drawing.Point(34, 334);
            this.txtRefxB.MaxLength = 6;
            this.txtRefxB.Name = "txtRefxB";
            this.txtRefxB.Size = new System.Drawing.Size(40, 20);
            this.txtRefxB.TabIndex = 39;
            this.txtRefxB.Text = "0.1427";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(17, 337);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(17, 13);
            this.label11.TabIndex = 38;
            this.label11.Text = "B:";
            // 
            // txtRefAbsIntG
            // 
            this.txtRefAbsIntG.Location = new System.Drawing.Point(121, 308);
            this.txtRefAbsIntG.MaxLength = 6;
            this.txtRefAbsIntG.Name = "txtRefAbsIntG";
            this.txtRefAbsIntG.Size = new System.Drawing.Size(55, 20);
            this.txtRefAbsIntG.TabIndex = 37;
            this.txtRefAbsIntG.Text = "2.20375";
            // 
            // txtRefyG
            // 
            this.txtRefyG.Location = new System.Drawing.Point(77, 308);
            this.txtRefyG.MaxLength = 6;
            this.txtRefyG.Name = "txtRefyG";
            this.txtRefyG.Size = new System.Drawing.Size(41, 20);
            this.txtRefyG.TabIndex = 36;
            this.txtRefyG.Text = "0.7177";
            // 
            // txtRefxG
            // 
            this.txtRefxG.Location = new System.Drawing.Point(34, 308);
            this.txtRefxG.MaxLength = 6;
            this.txtRefxG.Name = "txtRefxG";
            this.txtRefxG.Size = new System.Drawing.Size(40, 20);
            this.txtRefxG.TabIndex = 35;
            this.txtRefxG.Text = "0.1827";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(17, 311);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(18, 13);
            this.label10.TabIndex = 34;
            this.label10.Text = "G:";
            // 
            // txtRefAbsIntR
            // 
            this.txtRefAbsIntR.Location = new System.Drawing.Point(121, 282);
            this.txtRefAbsIntR.MaxLength = 6;
            this.txtRefAbsIntR.Name = "txtRefAbsIntR";
            this.txtRefAbsIntR.Size = new System.Drawing.Size(55, 20);
            this.txtRefAbsIntR.TabIndex = 33;
            this.txtRefAbsIntR.Text = "1.28569";
            // 
            // txtRefyR
            // 
            this.txtRefyR.Location = new System.Drawing.Point(77, 282);
            this.txtRefyR.MaxLength = 6;
            this.txtRefyR.Name = "txtRefyR";
            this.txtRefyR.Size = new System.Drawing.Size(41, 20);
            this.txtRefyR.TabIndex = 32;
            this.txtRefyR.Text = "0.2927";
            // 
            // txtRefxR
            // 
            this.txtRefxR.Location = new System.Drawing.Point(34, 282);
            this.txtRefxR.MaxLength = 6;
            this.txtRefxR.Name = "txtRefxR";
            this.txtRefxR.Size = new System.Drawing.Size(40, 20);
            this.txtRefxR.TabIndex = 31;
            this.txtRefxR.Text = "0.7071";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(17, 285);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(18, 13);
            this.label9.TabIndex = 30;
            this.label9.Text = "R:";
            // 
            // btnAdjustRGB
            // 
            this.btnAdjustRGB.Location = new System.Drawing.Point(17, 362);
            this.btnAdjustRGB.Name = "btnAdjustRGB";
            this.btnAdjustRGB.Size = new System.Drawing.Size(159, 27);
            this.btnAdjustRGB.TabIndex = 29;
            this.btnAdjustRGB.Text = "Adjust RGB";
            this.btnAdjustRGB.UseVisualStyleBackColor = true;
            this.btnAdjustRGB.Click += new System.EventHandler(this.btnAdjustRGB_Click);
            // 
            // btnReadParams
            // 
            this.btnReadParams.Location = new System.Drawing.Point(17, 407);
            this.btnReadParams.Name = "btnReadParams";
            this.btnReadParams.Size = new System.Drawing.Size(159, 27);
            this.btnReadParams.TabIndex = 14;
            this.btnReadParams.Text = "Read back params";
            this.btnReadParams.UseVisualStyleBackColor = true;
            this.btnReadParams.Click += new System.EventHandler(this.btnReadParams_Click);
            // 
            // txtRefy
            // 
            this.txtRefy.Location = new System.Drawing.Point(112, 207);
            this.txtRefy.MaxLength = 6;
            this.txtRefy.Name = "txtRefy";
            this.txtRefy.Size = new System.Drawing.Size(64, 20);
            this.txtRefy.TabIndex = 13;
            this.txtRefy.Text = "0.2912";
            // 
            // txtRefx
            // 
            this.txtRefx.Location = new System.Drawing.Point(48, 207);
            this.txtRefx.MaxLength = 6;
            this.txtRefx.Name = "txtRefx";
            this.txtRefx.Size = new System.Drawing.Size(58, 20);
            this.txtRefx.TabIndex = 12;
            this.txtRefx.Text = "0.7011";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(21, 210);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(27, 13);
            this.label7.TabIndex = 11;
            this.label7.Text = "Ref:";
            // 
            // txtRefWavelength
            // 
            this.txtRefWavelength.Location = new System.Drawing.Point(59, 136);
            this.txtRefWavelength.MaxLength = 3;
            this.txtRefWavelength.Name = "txtRefWavelength";
            this.txtRefWavelength.Size = new System.Drawing.Size(47, 20);
            this.txtRefWavelength.TabIndex = 10;
            this.txtRefWavelength.Text = "638";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(26, 140);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(27, 13);
            this.label6.TabIndex = 9;
            this.label6.Text = "Ref:";
            // 
            // txtRefAbsInt
            // 
            this.txtRefAbsInt.Location = new System.Drawing.Point(59, 65);
            this.txtRefAbsInt.MaxLength = 11;
            this.txtRefAbsInt.Name = "txtRefAbsInt";
            this.txtRefAbsInt.Size = new System.Drawing.Size(117, 20);
            this.txtRefAbsInt.TabIndex = 8;
            this.txtRefAbsInt.Text = "2.355E-02";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(26, 68);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(27, 13);
            this.label4.TabIndex = 7;
            this.label4.Text = "Ref:";
            // 
            // btnAdjustxy
            // 
            this.btnAdjustxy.Location = new System.Drawing.Point(17, 233);
            this.btnAdjustxy.Name = "btnAdjustxy";
            this.btnAdjustxy.Size = new System.Drawing.Size(159, 27);
            this.btnAdjustxy.TabIndex = 4;
            this.btnAdjustxy.Text = "Adjust xy";
            this.btnAdjustxy.UseVisualStyleBackColor = true;
            this.btnAdjustxy.Click += new System.EventHandler(this.btnAdjustxy_Click);
            // 
            // btnAdjustWavelength
            // 
            this.btnAdjustWavelength.Location = new System.Drawing.Point(17, 162);
            this.btnAdjustWavelength.Name = "btnAdjustWavelength";
            this.btnAdjustWavelength.Size = new System.Drawing.Size(159, 27);
            this.btnAdjustWavelength.TabIndex = 3;
            this.btnAdjustWavelength.Text = "Adjust Wavelength";
            this.btnAdjustWavelength.UseVisualStyleBackColor = true;
            this.btnAdjustWavelength.Click += new System.EventHandler(this.btnAdjustWavelength_Click);
            // 
            // btnAdjustAbsInt
            // 
            this.btnAdjustAbsInt.Location = new System.Drawing.Point(17, 91);
            this.btnAdjustAbsInt.Name = "btnAdjustAbsInt";
            this.btnAdjustAbsInt.Size = new System.Drawing.Size(159, 27);
            this.btnAdjustAbsInt.TabIndex = 2;
            this.btnAdjustAbsInt.Text = "Adjust Abs Int";
            this.btnAdjustAbsInt.UseVisualStyleBackColor = true;
            this.btnAdjustAbsInt.Click += new System.EventHandler(this.btnAdjustAbsInt_Click);
            // 
            // btnBalanceInt
            // 
            this.btnBalanceInt.Location = new System.Drawing.Point(17, 20);
            this.btnBalanceInt.Name = "btnBalanceInt";
            this.btnBalanceInt.Size = new System.Drawing.Size(159, 27);
            this.btnBalanceInt.TabIndex = 1;
            this.btnBalanceInt.Text = "Balance Int to avg";
            this.btnBalanceInt.UseVisualStyleBackColor = true;
            this.btnBalanceInt.Click += new System.EventHandler(this.btnBalanceInt_Click);
            // 
            // txtLog
            // 
            this.txtLog.Location = new System.Drawing.Point(399, 30);
            this.txtLog.Multiline = true;
            this.txtLog.Name = "txtLog";
            this.txtLog.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtLog.Size = new System.Drawing.Size(251, 426);
            this.txtLog.TabIndex = 7;
            // 
            // Label5
            // 
            this.Label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label5.Location = new System.Drawing.Point(396, 5);
            this.Label5.Name = "Label5";
            this.Label5.Size = new System.Drawing.Size(162, 22);
            this.Label5.TabIndex = 6;
            this.Label5.Text = "Results:";
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.label8);
            this.GroupBox1.Controls.Add(this.optFlash);
            this.GroupBox1.Controls.Add(this.optRAM);
            this.GroupBox1.Controls.Add(this.label2);
            this.GroupBox1.Controls.Add(this.lstCapture);
            this.GroupBox1.Controls.Add(this.chkPWM);
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Controls.Add(this.label3);
            this.GroupBox1.Controls.Add(this.numFibers);
            this.GroupBox1.Location = new System.Drawing.Point(12, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(183, 444);
            this.GroupBox1.TabIndex = 5;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications Setup";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(11, 241);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(102, 13);
            this.label8.TabIndex = 12;
            this.label8.Text = "Save parameters to:";
            // 
            // optFlash
            // 
            this.optFlash.AutoSize = true;
            this.optFlash.Location = new System.Drawing.Point(69, 257);
            this.optFlash.Name = "optFlash";
            this.optFlash.Size = new System.Drawing.Size(50, 17);
            this.optFlash.TabIndex = 11;
            this.optFlash.TabStop = true;
            this.optFlash.Text = "Flash";
            this.optFlash.UseVisualStyleBackColor = true;
            // 
            // optRAM
            // 
            this.optRAM.AutoSize = true;
            this.optRAM.Location = new System.Drawing.Point(14, 257);
            this.optRAM.Name = "optRAM";
            this.optRAM.Size = new System.Drawing.Size(49, 17);
            this.optRAM.TabIndex = 10;
            this.optRAM.TabStop = true;
            this.optRAM.Text = "RAM";
            this.optRAM.UseVisualStyleBackColor = true;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(9, 147);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(47, 13);
            this.label2.TabIndex = 9;
            this.label2.Text = "Capture:";
            // 
            // lstCapture
            // 
            this.lstCapture.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstCapture.FormattingEnabled = true;
            this.lstCapture.Items.AddRange(new object[] {
            "AUTO",
            "LOW",
            "MEDIUM",
            "HIGH",
            "SUPER",
            "ULTRA"});
            this.lstCapture.Location = new System.Drawing.Point(9, 167);
            this.lstCapture.Name = "lstCapture";
            this.lstCapture.Size = new System.Drawing.Size(135, 21);
            this.lstCapture.TabIndex = 8;
            // 
            // chkPWM
            // 
            this.chkPWM.AutoSize = true;
            this.chkPWM.Location = new System.Drawing.Point(9, 194);
            this.chkPWM.Name = "chkPWM";
            this.chkPWM.Size = new System.Drawing.Size(53, 17);
            this.chkPWM.TabIndex = 7;
            this.chkPWM.Text = "PWM";
            this.chkPWM.UseVisualStyleBackColor = true;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(6, 21);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(56, 13);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "Port:";
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(68, 18);
            this.lstPorts.Name = "lstPorts";
            this.lstPorts.Size = new System.Drawing.Size(64, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(9, 106);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(63, 13);
            this.label3.TabIndex = 6;
            this.label3.Text = "Fiber count:";
            // 
            // numFibers
            // 
            this.numFibers.Location = new System.Drawing.Point(78, 104);
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
            this.numFibers.Size = new System.Drawing.Size(48, 20);
            this.numFibers.TabIndex = 5;
            this.numFibers.Value = new decimal(new int[] {
            10,
            0,
            0,
            0});
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(667, 472);
            this.Controls.Add(this.txtLog);
            this.Controls.Add(this.Label5);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "UserCal demo - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numFibers)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.TextBox txtLog;
        internal System.Windows.Forms.Label Label5;
        internal System.Windows.Forms.Button btnBalanceInt;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
        private System.Windows.Forms.TextBox txtRefx;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox txtRefWavelength;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox txtRefAbsInt;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.NumericUpDown numFibers;
        internal System.Windows.Forms.Button btnAdjustxy;
        internal System.Windows.Forms.Button btnAdjustWavelength;
        internal System.Windows.Forms.Button btnAdjustAbsInt;
        private System.Windows.Forms.TextBox txtRefy;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox lstCapture;
        private System.Windows.Forms.CheckBox chkPWM;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.RadioButton optFlash;
        private System.Windows.Forms.RadioButton optRAM;
        internal System.Windows.Forms.Button btnReadParams;
        private System.Windows.Forms.TextBox txtRefAbsIntB;
        private System.Windows.Forms.TextBox txtRefyB;
        private System.Windows.Forms.TextBox txtRefxB;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox txtRefAbsIntG;
        private System.Windows.Forms.TextBox txtRefyG;
        private System.Windows.Forms.TextBox txtRefxG;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox txtRefAbsIntR;
        private System.Windows.Forms.TextBox txtRefyR;
        private System.Windows.Forms.TextBox txtRefxR;
        private System.Windows.Forms.Label label9;
        internal System.Windows.Forms.Button btnAdjustRGB;
    }
}

