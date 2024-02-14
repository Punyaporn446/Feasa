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
            this.dataGrid = new System.Windows.Forms.DataGridView();
            this.Red = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Green = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Blue = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Intensity = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.GroupBox2 = new System.Windows.Forms.GroupBox();
            this.cmdCapture = new System.Windows.Forms.Button();
            this.lstNumFibs = new System.Windows.Forms.ComboBox();
            this.Label6 = new System.Windows.Forms.Label();
            this.cmdRead = new System.Windows.Forms.Button();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.lstPorts = new System.Windows.Forms.ComboBox();
            ((System.ComponentModel.ISupportInitialize)(this.dataGrid)).BeginInit();
            this.GroupBox2.SuspendLayout();
            this.GroupBox1.SuspendLayout();
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
            this.Red,
            this.Green,
            this.Blue,
            this.Intensity});
            this.dataGrid.EnableHeadersVisualStyles = false;
            this.dataGrid.GridColor = System.Drawing.SystemColors.ButtonFace;
            this.dataGrid.Location = new System.Drawing.Point(11, 12);
            this.dataGrid.MultiSelect = false;
            this.dataGrid.Name = "dataGrid";
            this.dataGrid.ReadOnly = true;
            this.dataGrid.RowHeadersVisible = false;
            this.dataGrid.RowTemplate.Height = 18;
            this.dataGrid.Size = new System.Drawing.Size(256, 383);
            this.dataGrid.TabIndex = 13;
            // 
            // Red
            // 
            this.Red.HeaderText = "Red";
            this.Red.Name = "Red";
            this.Red.ReadOnly = true;
            this.Red.Width = 50;
            // 
            // Green
            // 
            this.Green.HeaderText = "Green";
            this.Green.Name = "Green";
            this.Green.ReadOnly = true;
            this.Green.Width = 50;
            // 
            // Blue
            // 
            this.Blue.HeaderText = "Blue";
            this.Blue.Name = "Blue";
            this.Blue.ReadOnly = true;
            this.Blue.Width = 50;
            // 
            // Intensity
            // 
            this.Intensity.HeaderText = "Intensity";
            this.Intensity.Name = "Intensity";
            this.Intensity.ReadOnly = true;
            // 
            // GroupBox2
            // 
            this.GroupBox2.Controls.Add(this.cmdCapture);
            this.GroupBox2.Controls.Add(this.lstNumFibs);
            this.GroupBox2.Controls.Add(this.Label6);
            this.GroupBox2.Controls.Add(this.cmdRead);
            this.GroupBox2.Location = new System.Drawing.Point(273, 76);
            this.GroupBox2.Name = "GroupBox2";
            this.GroupBox2.Size = new System.Drawing.Size(174, 319);
            this.GroupBox2.TabIndex = 12;
            this.GroupBox2.TabStop = false;
            this.GroupBox2.Text = "Commands";
            // 
            // cmdCapture
            // 
            this.cmdCapture.Location = new System.Drawing.Point(9, 21);
            this.cmdCapture.Name = "cmdCapture";
            this.cmdCapture.Size = new System.Drawing.Size(158, 29);
            this.cmdCapture.TabIndex = 17;
            this.cmdCapture.Text = "CAPTURE";
            this.cmdCapture.UseVisualStyleBackColor = true;
            this.cmdCapture.Click += new System.EventHandler(this.cmdCapture_Click);
            // 
            // lstNumFibs
            // 
            this.lstNumFibs.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstNumFibs.FormattingEnabled = true;
            this.lstNumFibs.Location = new System.Drawing.Point(83, 75);
            this.lstNumFibs.Name = "lstNumFibs";
			this.lstNumFibs.BackColor = System.Drawing.Color.White;
            this.lstNumFibs.Size = new System.Drawing.Size(64, 21);
            this.lstNumFibs.TabIndex = 16;
            // 
            // Label6
            // 
            this.Label6.AutoSize = true;
            this.Label6.Location = new System.Drawing.Point(6, 78);
            this.Label6.Name = "Label6";
            this.Label6.Size = new System.Drawing.Size(79, 13);
            this.Label6.TabIndex = 8;
            this.Label6.Text = "Fibers to Read:";
            // 
            // cmdRead
            // 
            this.cmdRead.Location = new System.Drawing.Point(9, 110);
            this.cmdRead.Name = "cmdRead";
            this.cmdRead.Size = new System.Drawing.Size(159, 27);
            this.cmdRead.TabIndex = 1;
            this.cmdRead.Text = "READ DATA";
            this.cmdRead.UseVisualStyleBackColor = true;
            this.cmdRead.Click += new System.EventHandler(this.cmdRead_Click);
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.Label1);
            this.GroupBox1.Controls.Add(this.lstPorts);
            this.GroupBox1.Location = new System.Drawing.Point(273, 12);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(174, 58);
            this.GroupBox1.TabIndex = 11;
            this.GroupBox1.TabStop = false;
            this.GroupBox1.Text = "Communications Setup";
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(21, 27);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(56, 13);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "Port:";
            // 
            // lstPorts
            // 
            this.lstPorts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.lstPorts.FormattingEnabled = true;
            this.lstPorts.Location = new System.Drawing.Point(83, 24);
			this.lstPorts.Name = "lstPorts";
			this.lstPorts.BackColor = System.Drawing.Color.White;
            this.lstPorts.Size = new System.Drawing.Size(84, 21);
            this.lstPorts.TabIndex = 2;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(460, 406);
            this.Controls.Add(this.dataGrid);
            this.Controls.Add(this.GroupBox2);
            this.Controls.Add(this.GroupBox1);
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "get RGBI - (c) Feasa Enterprises Ltd";
            this.Load += new System.EventHandler(this.Form1_Load);
            ((System.ComponentModel.ISupportInitialize)(this.dataGrid)).EndInit();
            this.GroupBox2.ResumeLayout(false);
            this.GroupBox2.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.DataGridView dataGrid;
        internal System.Windows.Forms.DataGridViewTextBoxColumn Red;
        internal System.Windows.Forms.DataGridViewTextBoxColumn Green;
        internal System.Windows.Forms.DataGridViewTextBoxColumn Blue;
        internal System.Windows.Forms.DataGridViewTextBoxColumn Intensity;
        internal System.Windows.Forms.GroupBox GroupBox2;
        internal System.Windows.Forms.Button cmdCapture;
        internal System.Windows.Forms.ComboBox lstNumFibs;
        internal System.Windows.Forms.Label Label6;
        internal System.Windows.Forms.Button cmdRead;
        internal System.Windows.Forms.GroupBox GroupBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox lstPorts;
    }
}

