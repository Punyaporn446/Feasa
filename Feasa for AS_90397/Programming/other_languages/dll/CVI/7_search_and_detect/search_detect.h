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
#define  PANEL_BTNDETECT                  2       /* control type: command, callback function: btnAction */
#define  PANEL_BTNFIND                    3       /* control type: command, callback function: btnAction */
#define  PANEL_TEXTBOX                    4       /* control type: textBox, callback function: (none) */
#define  PANEL_DECORATION                 5       /* control type: deco, callback function: (none) */
#define  PANEL_TXTLOG                     6       /* control type: textBox, callback function: (none) */
#define  PANEL_TXTSN                      7       /* control type: string, callback function: (none) */


     /* Control Arrays: */

          /* (no control arrays in the resource file) */


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnAction(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
