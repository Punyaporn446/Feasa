/**************************************************************************/
/* LabWindows/CVI User Interface Resource (UIR) Include File              */
/* Copyright (c) National Instruments 2020. All Rights Reserved.          */
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
#define  PANEL_BTNSEQUENCETEST            2       /* control type: command, callback function: btnCaptureSequence */
#define  PANEL_BTNFINDPARAMS              3       /* control type: command, callback function: btnFindParams */
#define  PANEL_TEXTBOX                    4       /* control type: textBox, callback function: (none) */
#define  PANEL_DECORATION_2               5       /* control type: deco, callback function: (none) */
#define  PANEL_DECORATION                 6       /* control type: deco, callback function: (none) */
#define  PANEL_LSTSIGNALSPEED             7       /* control type: ring, callback function: (none) */
#define  PANEL_LSTBLINKINGSPEED           8       /* control type: ring, callback function: (none) */
#define  PANEL_LSTCOMMPORT                9       /* control type: ring, callback function: (none) */
#define  PANEL_TXTLEDCOUNT                10      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTSAMPLES                 11      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTWAITTIME                12      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTCAPTURETIME             13      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTSTARTDELAY              14      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTFIBERFIND               15      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTFIBER                   16      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTCYCLES                  17      /* control type: numeric, callback function: (none) */
#define  PANEL_CHKTIMERESIMPORTANT        18      /* control type: radioButton, callback function: (none) */
#define  PANEL_GRAPHCIE                   19      /* control type: graph, callback function: (none) */
#define  PANEL_GRAPHINT                   20      /* control type: graph, callback function: (none) */


     /* Control Arrays: */

          /* (no control arrays in the resource file) */


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnCaptureSequence(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK btnFindParams(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
