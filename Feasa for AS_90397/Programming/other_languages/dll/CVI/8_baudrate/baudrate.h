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
#define  PANEL_BTNCAPTUREANDREAD          2       /* control type: command, callback function: btnCaptureAndRead */
#define  PANEL_TEXTBOX                    3       /* control type: textBox, callback function: (none) */
#define  PANEL_DECORATION                 4       /* control type: deco, callback function: (none) */
#define  PANEL_TEXTMSG                    5       /* control type: textMsg, callback function: (none) */
#define  PANEL_LSTCOMMPORT                6       /* control type: ring, callback function: (none) */
#define  PANEL_TEXTMSG_2                  7       /* control type: textMsg, callback function: (none) */
#define  PANEL_NUMFIBER                   8       /* control type: numeric, callback function: (none) */
#define  PANEL_TEXTMSG_3                  9       /* control type: textMsg, callback function: (none) */
#define  PANEL_LSTBAUDRATE                10      /* control type: ring, callback function: (none) */
#define  PANEL_TXTLOG                     11      /* control type: textBox, callback function: (none) */
#define  PANEL_OPTBAUDRATE                12      /* control type: tree, callback function: (none) */
#define  PANEL_TEXTMSG_4                  13      /* control type: textMsg, callback function: (none) */


     /* Control Arrays: */

          /* (no control arrays in the resource file) */


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnCaptureAndRead(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
