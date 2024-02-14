/**************************************************************************/
/* LabWindows/CVI User Interface Resource (UIR) Include File              */
/* Copyright (c) National Instruments 2011. All Rights Reserved.          */
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
#define  PANEL_DECORATION                 4
#define  PANEL_TEXTMSG                    5
#define  PANEL_LSTCOMMPORT                6
#define  PANEL_TEXTMSG_2                  7
#define  PANEL_NUMFIBER                   8
#define  PANEL_TEXTMSG_3                  9
#define  PANEL_INT                        10
#define  PANEL_SAT                        11
#define  PANEL_RED                        12
#define  PANEL_BLUE                       13
#define  PANEL_GREEN                      14
#define  PANEL_HUE                        15


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnCaptureAndRead(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
