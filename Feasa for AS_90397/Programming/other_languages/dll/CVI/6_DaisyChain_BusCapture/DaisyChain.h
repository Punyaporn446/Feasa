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
#define  PANEL_BTNADDANALYSER             2       /* callback function: btnAddAnalyser */
#define  PANEL_BTNCAPTUREANDREAD          3       /* callback function: btnCaptureAndRead */
#define  PANEL_TXTRESULT                  4
#define  PANEL_TEXTBOX                    5
#define  PANEL_DECORATION                 6
#define  PANEL_LSTCOMMPORT                7
#define  PANEL_TEXTMSG                    8
#define  PANEL_LSTCAPTURERANGE            9
#define  PANEL_LSTCAPTUREMODE             10
#define  PANEL_NUMFIBER                   11
#define  PANEL_TEXTMSG_2                  12
#define  PANEL_LSTANALYSERS               13


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnAddAnalyser(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK btnCaptureAndRead(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
