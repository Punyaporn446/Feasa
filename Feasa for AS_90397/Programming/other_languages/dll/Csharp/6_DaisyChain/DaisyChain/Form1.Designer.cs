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
            this.txtFiber = new System.Windows.Forms.TextBox();
            this.Label6 = new System.Windows.Forms.Label();
            this.cmdRead = new System.Windows.Forms.Button();
            this.GroupBox3 = new System.Windows.Forms.GroupBox();
            this.txtLog = new System.Windows.Forms.TextBox();
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.cmdFree = new System.Windows.Forms.Button();
            this.cmdGet = new System.Windows.Forms.Button();
            this.txtSN = new System.Windows.Forms.TextBox();
            this.Label4 = new System.Windows.Forms.Label();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.Label3 = new System.Windows.Forms.Label();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            this.GroupBox4.SuspendLayout();
            this.GroupBox3.SuspendLayout();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox4
            // 
            this.GroupBox4.Controls.Add(this.txtFiber);
            this.GroupBox4.Controls.Add(this.Label6);
            this.GroupBox4.Controls.Add(this.cmdRead);
            this.GroupBox4.Location = new System.Drawing.Point(204, 146);
            this.GroupBox4.Name = "GroupBox4";
            this.GroupBox4.Size = new System.Drawing.Size(173, 101);
            this.GroupBox4.TabIndex = 10;
            this.GroupBox4.TabStop = false;
            this.GroupBox4.Text = "Measure";
            // 
            // txtFiber
            // 
            this.txtFiber.Location = new System.Drawing.Point(85, 22);
            this.txtFiber.MaxLength = 2;
            this.txtFiber.Name = "txtFiber";
            this.txtFiber.Size = new System.Drawing.Size(30, 20);
            this.txtFiber.TabIndex = 12;
            this.txtFiber.Text = "1";
            // 
            // Label6
            // 
            this.Label6.AutoSize = true;
            this.Label6.Location = new System.Drawing.Point(5, 25);
            this.Label6.Name = "Label6";
            this.Label6.Size = new System.Drawing.Size(74, 13);
            this.Label6.TabIndex = 11;
            this.Label6.Text = "Fiber to Read:";
            // 
            // cmdRead
            // 
            this.cmdRead.Location = new System.Drawing.Point(8, 52);
            this.cmdRead.Name = "cmdRead";
            this.cmdRead.Size = new System.Drawing.Size(159, 27);
            this.cmdRead.TabIndex = 10;
            this.cmdRead.Text = "CAPTURE AND READ";
            this.cmdRead.UseVisualStyleBackColor = true;
            this.cmdRead.Click += new System.EventHandler(this.cmdRead_Click);
            // 
            // GroupBox3
            // 
            this.GroupBox3.Controls.Add(this.txtLog);
            this.GroupBox3.Location = new System.Drawing.Point(203, 253);
            this.GroupBox3.Name = "GroupBox3";
            this.GroupBox3.Size = new System.Drawing.Size(173, 101);
            this.GroupBox3.TabIndex = 9;
            this.GroupBox3.TabStop = false;
            this.GroupBox3.Text = "Response";
            // 
            // txtLog
            // 
            this.txtLog.Location = new System.Drawing.Point(9, 19);
            this.txtLog.Multiline = true;
            this.txtLog.Name = "txtLog";
            this.txtLog.Size = new System.Drawing.Size(154, 73);
            this.txtLog.TabIndex = 8;
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.cmdFree);
            this.GroupBox2.Controls.Add(this.cmdGet);
            this.GroupBox2.Controls.Add(this.txtSN);
            this.GroupBox2.Controls.Add(this.Label4);
            this.GroupBox2.Location = new System.Drawing.Point(203, 12);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(174, 128);
            this.GroupBox2.TabIndex = 8;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Daisy Chain";
            // 
            // cmdFree
            // 
            this.cmdFree.Location = new System.Drawing.Point(93, 88);
            this.cmdFree.Name = "cmdFree";
            this.cmdFree.Size = new System.Drawing.Size(70, 25);
            this.cmdFree.TabIndex = 15;
            this.cmdFree.Text = "FREE BUS";
            this.cmdFree.UseVisualStyleBackColor = true;
            this.cmdFree.Click += new System.EventHandler(this.cmdFree_Click);
            // 
            // cmdGet
            // 
            this.cmdGet.Location = new System.Drawing.Point(9, 88);
            this.cmdGet.Name = "cmdGet";
            this.cmdGet.Size = new System.Drawing.Size(70, 25);
            this.cmdGet.TabIndex = 14;
            this.cmdGet.Text = "GET BUS";
            this.cmdGet.UseVisualStyleBackColor = true;
            this.cmdGet.Click += new System.EventHandler(this.cmdGet_Click);
            // 
            // txtSN
            // 
            this.txtSN.Location = new System.Drawing.Point(9, 61);
            this.txtSN.MaxLength = 4;
            this.txtSN.Name = "txtSN";
            this.txtSN.Size = new System.Drawing.Size(44, 20);
            this.txtSN.TabIndex = 13;
            this.txtSN.Text = "E001";
            // 
            // Label4
            // 
            this.Label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label4.Location = new System.Drawing.Point(6, 21);
            this.Label4.Name = "Label4";
            this.Label4.Size = new System.Drawing.Size(162, 37);
            this.Label4.TabIndex = 5;
            this.Label4.Text = "Serial Number of the LED Analyser you want to control:";
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.Label3);
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(14, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(183, 342);
            this.GroupBox1.TabIndex = 7;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications Setup";
            // 
            // Label3
            // 
            this.Label3.Location = new System.Drawing.Point(6, 81);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(171, 85);
            this.Label3.TabIndex = 5;
            this.Label3.Text = "First, you need to select the communications port where the 1st LED Analyser of t" +
                "he BUS is connected. This will be the port through which you can access to the b" +
                "us";
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(6, 21);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(167, 13);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "COM Port of the 1st LED Analyser:";
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(109, 37);
            this.lstPorts.Name = "lstPorts";
            this.lstPorts.Size = new System.Drawing.Size(64, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(386, 363);
            this.Controls.Add(this.GroupBox4);
            this.Controls.Add(this.GroupBox3);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Daisy Chain - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox4.ResumeLayout(false);
            this.GroupBox4.PerformLayout();
            this.GroupBox3.ResumeLayout(false);
            this.GroupBox3.PerformLayout();
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.GroupBox GroupBox4;
        internal System.Windows.Forms.TextBox txtFiber;
        internal System.Windows.Forms.Label Label6;
        internal System.Windows.Forms.Button cmdRead;
        internal System.Windows.Forms.GroupBox GroupBox3;
        internal System.Windows.Forms.TextBox txtLog;
        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.Button cmdFree;
        internal System.Windows.Forms.Button cmdGet;
        internal System.Windows.Forms.TextBox txtSN;
        internal System.Windows.Forms.Label Label4;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label3;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
    }
}

