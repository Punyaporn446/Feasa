namespace Baudrate
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
            this.txtFiber = new System.Windows.Forms.TextBox();
            this.Label6 = new System.Windows.Forms.Label();
            this.txtLog = new System.Windows.Forms.TextBox();
            this.Label5 = new System.Windows.Forms.Label();
            this.Label4 = new System.Windows.Forms.Label();
            this.cmdRead = new System.Windows.Forms.Button();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.lstBaudrate = new System.Windows.Forms.ComboBox();
            this.optBaudrateManual = new System.Windows.Forms.RadioButton();
            this.optBaudrateAuto = new System.Windows.Forms.RadioButton();
            this.Label3 = new System.Windows.Forms.Label();
            this.Label2 = new System.Windows.Forms.Label();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.txtFiber);
            this.GroupBox2.Controls.Add(this.Label6);
            this.GroupBox2.Controls.Add(this.txtLog);
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
            // txtFiber
            // 
            this.txtFiber.Location = new System.Drawing.Point(86, 81);
            this.txtFiber.MaxLength = 2;
            this.txtFiber.Name = "txtFiber";
			this.txtFiber.BackColor = System.Drawing.Color.White;
            this.txtFiber.Size = new System.Drawing.Size(30, 20);
            this.txtFiber.TabIndex = 9;
            this.txtFiber.Text = "1";
            // 
            // Label6
            // 
            this.Label6.AutoSize = true;
            this.Label6.Location = new System.Drawing.Point(6, 84);
            this.Label6.Name = "Label6";
            this.Label6.Size = new System.Drawing.Size(74, 13);
            this.Label6.TabIndex = 8;
            this.Label6.Text = "Fiber to Read:";
            // 
            // txtLog
            // 
            this.txtLog.Location = new System.Drawing.Point(9, 178);
            this.txtLog.Multiline = true;
			this.txtLog.Name = "txtLog";
			this.txtLog.BackColor = System.Drawing.Color.White;
            this.txtLog.Size = new System.Drawing.Size(154, 106);
            this.txtLog.TabIndex = 7;
            // 
            // Label5
            // 
            this.Label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label5.Location = new System.Drawing.Point(6, 153);
            this.Label5.Name = "Label5";
            this.Label5.Size = new System.Drawing.Size(162, 22);
            this.Label5.TabIndex = 6;
            this.Label5.Text = "Response:";
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
            this.cmdRead.Location = new System.Drawing.Point(9, 111);
            this.cmdRead.Name = "cmdRead";
            this.cmdRead.Size = new System.Drawing.Size(159, 27);
            this.cmdRead.TabIndex = 1;
            this.cmdRead.Text = "CAPTURE AND READ";
            this.cmdRead.UseVisualStyleBackColor = true;
            this.cmdRead.Click += new System.EventHandler(this.cmdRead_Click);
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.lstBaudrate);
            this.GroupBox1.Controls.Add(this.optBaudrateManual);
            this.GroupBox1.Controls.Add(this.optBaudrateAuto);
            this.GroupBox1.Controls.Add(this.Label3);
            this.GroupBox1.Controls.Add(this.Label2);
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(12, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(183, 295);
            this.GroupBox1.TabIndex = 5;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications Setup";
            // 
            // lstBaudrate
            // 
            this.lstBaudrate.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstBaudrate.Enabled = false;
            this.lstBaudrate.FormattingEnabled = true;
            this.lstBaudrate.Location = new System.Drawing.Point(82, 98);
			this.lstBaudrate.Name = "lstBaudrate";
			this.lstBaudrate.BackColor = System.Drawing.Color.White;
            this.lstBaudrate.Size = new System.Drawing.Size(74, 21);
            this.lstBaudrate.TabIndex = 10;
            // 
            // optBaudrateManual
            // 
            this.optBaudrateManual.AutoSize = true;
            this.optBaudrateManual.Location = new System.Drawing.Point(20, 99);
            this.optBaudrateManual.Name = "optBaudrateManual";
            this.optBaudrateManual.Size = new System.Drawing.Size(66, 17);
            this.optBaudrateManual.TabIndex = 9;
            this.optBaudrateManual.Text = "Manual: ";
            this.optBaudrateManual.UseVisualStyleBackColor = true;
            this.optBaudrateManual.CheckedChanged += new System.EventHandler(this.optBaudrateManual_CheckedChanged);
            // 
            // optBaudrateAuto
            // 
            this.optBaudrateAuto.AutoSize = true;
            this.optBaudrateAuto.Checked = true;
            this.optBaudrateAuto.Location = new System.Drawing.Point(20, 79);
            this.optBaudrateAuto.Name = "optBaudrateAuto";
            this.optBaudrateAuto.Size = new System.Drawing.Size(47, 17);
            this.optBaudrateAuto.TabIndex = 8;
            this.optBaudrateAuto.TabStop = true;
            this.optBaudrateAuto.Text = "Auto";
            this.optBaudrateAuto.UseVisualStyleBackColor = true;
            this.optBaudrateAuto.CheckedChanged += new System.EventHandler(this.optBaudrateAuto_CheckedChanged);
            // 
            // Label3
            // 
            this.Label3.AutoSize = true;
            this.Label3.Location = new System.Drawing.Point(6, 63);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(53, 13);
            this.Label3.TabIndex = 7;
            this.Label3.Text = "Baudrate:";
            // 
            // Label2
            // 
            this.Label2.Location = new System.Drawing.Point(6, 168);
            this.Label2.Name = "Label2";
            this.Label2.Size = new System.Drawing.Size(171, 116);
            this.Label2.TabIndex = 6;
            this.Label2.Text = resources.GetString("Label2.Text");
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
            this.ClientSize = new System.Drawing.Size(387, 318);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Baudrate - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.TextBox txtFiber;
        internal System.Windows.Forms.Label Label6;
        internal System.Windows.Forms.TextBox txtLog;
        internal System.Windows.Forms.Label Label5;
        internal System.Windows.Forms.Label Label4;
        internal System.Windows.Forms.Button cmdRead;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.ComboBox lstBaudrate;
        internal System.Windows.Forms.RadioButton optBaudrateManual;
        internal System.Windows.Forms.RadioButton optBaudrateAuto;
        internal System.Windows.Forms.Label Label3;
        internal System.Windows.Forms.Label Label2;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;

    }
}

