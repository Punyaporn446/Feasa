namespace getHSI_simple
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
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.lblIntensity = new System.Windows.Forms.Label();
            this.lblSat = new System.Windows.Forms.Label();
            this.lblHue = new System.Windows.Forms.Label();
            this.Label8 = new System.Windows.Forms.Label();
            this.Label7 = new System.Windows.Forms.Label();
            this.txtFiber = new System.Windows.Forms.TextBox();
            this.Label6 = new System.Windows.Forms.Label();
            this.Label5 = new System.Windows.Forms.Label();
            this.Label4 = new System.Windows.Forms.Label();
            this.cmdRead = new System.Windows.Forms.Button();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.lblIntensity);
            this.GroupBox2.Controls.Add(this.lblSat);
            this.GroupBox2.Controls.Add(this.lblHue);
            this.GroupBox2.Controls.Add(this.Label8);
            this.GroupBox2.Controls.Add(this.Label7);
            this.GroupBox2.Controls.Add(this.txtFiber);
            this.GroupBox2.Controls.Add(this.Label6);
            this.GroupBox2.Controls.Add(this.Label5);
            this.GroupBox2.Controls.Add(this.Label4);
            this.GroupBox2.Controls.Add(this.cmdRead);
            this.GroupBox2.Location = new System.Drawing.Point(201, 12);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(174, 295);
            this.GroupBox2.TabIndex = 6;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Commands";
            // 
            // lblIntensity
            // 
            this.lblIntensity.BackColor = System.Drawing.Color.White;
            this.lblIntensity.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.lblIntensity.Location = new System.Drawing.Point(113, 256);
            this.lblIntensity.Name = "lblIntensity";
            this.lblIntensity.Size = new System.Drawing.Size(54, 20);
            this.lblIntensity.TabIndex = 15;
            this.lblIntensity.Text = "0";
            this.lblIntensity.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // lblSat
            // 
            this.lblSat.BackColor = System.Drawing.Color.White;
            this.lblSat.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.lblSat.Location = new System.Drawing.Point(113, 233);
            this.lblSat.Name = "lblSat";
            this.lblSat.Size = new System.Drawing.Size(54, 20);
            this.lblSat.TabIndex = 14;
            this.lblSat.Text = "0";
            this.lblSat.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // lblHue
            // 
            this.lblHue.BackColor = System.Drawing.Color.White;
            this.lblHue.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.lblHue.Location = new System.Drawing.Point(113, 210);
            this.lblHue.Name = "lblHue";
            this.lblHue.Size = new System.Drawing.Size(54, 20);
            this.lblHue.TabIndex = 13;
            this.lblHue.Text = "0.0";
            this.lblHue.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // Label8
            // 
            this.Label8.AutoSize = true;
            this.Label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label8.Location = new System.Drawing.Point(59, 256);
            this.Label8.Name = "Label8";
            this.Label8.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.Label8.Size = new System.Drawing.Size(54, 15);
            this.Label8.TabIndex = 12;
            this.Label8.Text = "Intensity:";
            // 
            // Label7
            // 
            this.Label7.AutoSize = true;
            this.Label7.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label7.Location = new System.Drawing.Point(47, 233);
            this.Label7.Name = "Label7";
            this.Label7.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.Label7.Size = new System.Drawing.Size(66, 15);
            this.Label7.TabIndex = 10;
            this.Label7.Text = "Saturation:";
            // 
            // txtFiber
            // 
            this.txtFiber.Location = new System.Drawing.Point(86, 113);
            this.txtFiber.MaxLength = 2;
            this.txtFiber.Name = "txtFiber";
            this.txtFiber.Size = new System.Drawing.Size(30, 20);
			this.txtFiber.BackColor = System.Drawing.Color.White;
            this.txtFiber.TabIndex = 9;
            this.txtFiber.Text = "1";
            // 
            // Label6
            // 
            this.Label6.AutoSize = true;
            this.Label6.Location = new System.Drawing.Point(6, 116);
            this.Label6.Name = "Label6";
            this.Label6.Size = new System.Drawing.Size(74, 13);
            this.Label6.TabIndex = 8;
            this.Label6.Text = "Fiber to Read:";
            // 
            // Label5
            // 
            this.Label5.AutoSize = true;
            this.Label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label5.Location = new System.Drawing.Point(80, 210);
            this.Label5.Name = "Label5";
            this.Label5.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.Label5.Size = new System.Drawing.Size(33, 15);
            this.Label5.TabIndex = 6;
            this.Label5.Text = "Hue:";
            // 
            // Label4
            // 
            this.Label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label4.Location = new System.Drawing.Point(6, 21);
            this.Label4.Name = "Label4";
            this.Label4.Size = new System.Drawing.Size(162, 53);
            this.Label4.TabIndex = 5;
            this.Label4.Text = "Press the button shown below to execute a capture and read the result.";
            // 
            // cmdRead
            // 
            this.cmdRead.Location = new System.Drawing.Point(9, 148);
            this.cmdRead.Name = "cmdRead";
            this.cmdRead.Size = new System.Drawing.Size(159, 27);
            this.cmdRead.TabIndex = 1;
            this.cmdRead.Text = "CAPTURE AND READ";
            this.cmdRead.UseVisualStyleBackColor = true;
            this.cmdRead.Click += new System.EventHandler(this.cmdRead_Click);
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(12, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(183, 295);
            this.GroupBox1.TabIndex = 5;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications Setup";
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
			this.lstPorts.BackColor = System.Drawing.Color.White;
            this.lstPorts.Size = new System.Drawing.Size(84, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(389, 320);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Get HSI simple - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.Label lblIntensity;
        internal System.Windows.Forms.Label lblSat;
        internal System.Windows.Forms.Label lblHue;
        internal System.Windows.Forms.Label Label8;
        internal System.Windows.Forms.Label Label7;
        internal System.Windows.Forms.TextBox txtFiber;
        internal System.Windows.Forms.Label Label6;
        internal System.Windows.Forms.Label Label5;
        internal System.Windows.Forms.Label Label4;
        internal System.Windows.Forms.Button cmdRead;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
    }
}

