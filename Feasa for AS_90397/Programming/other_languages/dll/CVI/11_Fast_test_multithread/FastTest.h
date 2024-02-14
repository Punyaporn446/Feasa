/**************************************************************************/
/* LabWindows/CVI User Interface Resource (UIR) Include File              */
/* Copyright (c) National Instruments 2019. All Rights Reserved.          */
/*                                                                        */
/* WARNING: Do not add to, delete from, or otherwise modify the contents  */
/*          of this include file.                                         */
/**************************************************************************/

#include <userint.h>

#ifdef __cplusplus
    extern "C" {
#endif

     /* Panels and Controls: */

#define  PANEL                            1       /* callback function: panelCB */
#define  PANEL_BTNADDANALYSER             2       /* control type: command, callback function: btnAddAnalyser */
#define  PANEL_BTNCAPTUREANDREAD          3       /* control type: command, callback function: btnCaptureAndRead */
#define  PANEL_TXTRESULTS                 4       /* control type: textBox, callback function: (none) */
#define  PANEL_TEXTBOX                    5       /* control type: textBox, callback function: (none) */
#define  PANEL_DECORATION                 6       /* control type: deco, callback function: (none) */
#define  PANEL_LSTCOMMPORT                7       /* control type: ring, callback function: (none) */
#define  PANEL_TXTEXECTIME                8       /* control type: textMsg, callback function: (none) */
#define  PANEL_LSTPORTSTOTEST             9       /* control type: listBox, callback function: (none) */


     /* Control Arrays: */

          /* (no control arrays in the resource file) */


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnAddAnalyser(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK btnCaptureAndRead(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
