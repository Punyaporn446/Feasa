namespace Search_LedAnalyser
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
            this.GroupBox3 = new System.Windows.Forms.GroupBox();
            this.txtLog = new System.Windows.Forms.TextBox();
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.cmdSearch = new System.Windows.Forms.Button();
            this.txtSN = new System.Windows.Forms.TextBox();
            this.Label4 = new System.Windows.Forms.Label();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.btnDetectSerials = new System.Windows.Forms.Button();
            this.btnDetectPorts = new System.Windows.Forms.Button();
            this.Label3 = new System.Windows.Forms.Label();
            this.GroupBox3.SuspendLayout();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox3
            // 
            this.GroupBox3.Controls.Add(this.txtLog);
            this.GroupBox3.Location = new System.Drawing.Point(195, 124);
            this.GroupBox3.Name = "GroupBox3";
            this.GroupBox3.Size = new System.Drawing.Size(180, 230);
            this.GroupBox3.TabIndex = 8;
            this.GroupBox3.TabStop = false;
            this.GroupBox3.Text = "Status";
            // 
            // txtLog
            // 
            this.txtLog.Location = new System.Drawing.Point(6, 19);
            this.txtLog.Multiline = true;
            this.txtLog.Name = "txtLog";
            this.txtLog.Size = new System.Drawing.Size(168, 205);
            this.txtLog.TabIndex = 8;
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.cmdSearch);
            this.GroupBox2.Controls.Add(this.txtSN);
            this.GroupBox2.Controls.Add(this.Label4);
            this.GroupBox2.Location = new System.Drawing.Point(195, 12);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(180, 106);
            this.GroupBox2.TabIndex = 7;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Search";
            // 
            // cmdSearch
            // 
            this.cmdSearch.Location = new System.Drawing.Point(9, 47);
            this.cmdSearch.Name = "cmdSearch";
            this.cmdSearch.Size = new System.Drawing.Size(160, 43);
            this.cmdSearch.TabIndex = 14;
            this.cmdSearch.Text = "FIND LED ANALYSER && SHOW THE STATUS";
            this.cmdSearch.UseVisualStyleBackColor = true;
            this.cmdSearch.Click += new System.EventHandler(this.cmdSearch_Click);
            // 
            // txtSN
            // 
            this.txtSN.Location = new System.Drawing.Point(125, 21);
            this.txtSN.MaxLength = 4;
            this.txtSN.Name = "txtSN";
            this.txtSN.Size = new System.Drawing.Size(44, 20);
            this.txtSN.TabIndex = 13;
            this.txtSN.Text = "E001";
            // 
            // Label4
            // 
            this.Label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label4.Location = new System.Drawing.Point(6, 21);
            this.Label4.Name = "Label4";
            this.Label4.Size = new System.Drawing.Size(118, 20);
            this.Label4.TabIndex = 5;
            this.Label4.Text = "Serial Number to Find:";
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.btnDetectSerials);
            this.GroupBox1.Controls.Add(this.btnDetectPorts);
            this.GroupBox1.Controls.Add(this.Label3);
            this.GroupBox1.Location = new System.Drawing.Point(12, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(173, 342);
            this.GroupBox1.TabIndex = 6;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Info";
            // 
            // btnDetectSerials
            // 
            this.btnDetectSerials.Location = new System.Drawing.Point(7, 293);
            this.btnDetectSerials.Name = "btnDetectSerials";
            this.btnDetectSerials.Size = new System.Drawing.Size(160, 43);
            this.btnDetectSerials.TabIndex = 16;
            this.btnDetectSerials.Text = "Detect Serials";
            this.btnDetectSerials.UseVisualStyleBackColor = true;
            this.btnDetectSerials.Click += new System.EventHandler(this.btnDetectSerials_Click);
            // 
            // btnDetectPorts
            // 
            this.btnDetectPorts.Location = new System.Drawing.Point(6, 244);
            this.btnDetectPorts.Name = "btnDetectPorts";
            this.btnDetectPorts.Size = new System.Drawing.Size(160, 43);
            this.btnDetectPorts.TabIndex = 15;
            this.btnDetectPorts.Text = "Detect Ports";
            this.btnDetectPorts.UseVisualStyleBackColor = true;
            this.btnDetectPorts.Click += new System.EventHandler(this.btnDetect_Click);
            // 
            // Label3
            // 
            this.Label3.Location = new System.Drawing.Point(6, 21);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(161, 142);
            this.Label3.TabIndex = 5;
            this.Label3.Text = resources.GetString("Label3.Text");
            this.Label3.Click += new System.EventHandler(this.Label3_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(386, 366);
            this.Controls.Add(this.GroupBox3);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Search & Detect - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.GroupBox3.ResumeLayout(false);
            this.GroupBox3.PerformLayout();
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.GroupBox GroupBox3;
        internal System.Windows.Forms.TextBox txtLog;
        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.Button cmdSearch;
        internal System.Windows.Forms.TextBox txtSN;
        internal System.Windows.Forms.Label Label4;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label3;
        internal System.Windows.Forms.Button btnDetectPorts;
        internal System.Windows.Forms.Button btnDetectSerials;
    }
}

