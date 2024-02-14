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
#define  PANEL_HUE                        12
#define  PANEL_BTNFREEBUS                 13      /* callback function: btnFreeBus */
#define  PANEL_BTNGETBUS                  14      /* callback function: btnGetBus */
#define  PANEL_TXTSERIALNUMBER            15
#define  PANEL_DECORATION_2               16


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnCaptureAndRead(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK btnFreeBus(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK btnGetBus(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
