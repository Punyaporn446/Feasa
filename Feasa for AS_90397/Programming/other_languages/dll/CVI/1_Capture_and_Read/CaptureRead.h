/**************************************************************************/
/* LabWindows/CVI User Interface Resource (UIR) Include File              */
/* Copyright (c) National Instruments 2016. All Rights Reserved.          */
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
#define  PANEL_BTNCAPTUREANDREAD          2       /* callback function: btnCaptureAndRead */
#define  PANEL_TEXTBOX                    3
#define  PANEL_TXTRESULT                  4
#define  PANEL_DECORATION                 5
#define  PANEL_TEXTMSG                    6
#define  PANEL_LSTCOMMPORT                7
#define  PANEL_TEXTMSG_2                  8
#define  PANEL_NUMFIBER                   9
#define  PANEL_TEXTMSG_3                  10


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnCaptureAndRead(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
