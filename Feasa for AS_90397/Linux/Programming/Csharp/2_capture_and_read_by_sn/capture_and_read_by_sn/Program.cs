﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace capture_and_read_by_sn
{
    static class Program
    {
        /// <summary>
        /// Punto de entrada principal para la aplicación.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }
    }
}
