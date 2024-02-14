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
#define  PANEL_TEXTBOX                    3       /* control type: textBox, callback function: (none) */
#define  PANEL_DECORATION                 4       /* control type: deco, callback function: (none) */
#define  PANEL_LSTCOMMPORT                5       /* control type: ring, callback function: (none) */
#define  PANEL_TXTSAMPLES                 6       /* control type: numeric, callback function: (none) */
#define  PANEL_TXTWAITTIME                7       /* control type: numeric, callback function: (none) */
#define  PANEL_TXTCAPTURETIME             8       /* control type: numeric, callback function: (none) */
#define  PANEL_TXTSTARTDELAY              9       /* control type: numeric, callback function: (none) */
#define  PANEL_TXTFIBERSTOTEST            10      /* control type: numeric, callback function: (none) */
#define  PANEL_CHKISOFFTOON               11      /* control type: radioButton, callback function: (none) */
#define  PANEL_GRAPHINT                   12      /* control type: graph, callback function: (none) */
#define  PANEL_TABLETIMING                13      /* control type: table, callback function: (none) */
#define  PANEL_TEXTMSG                    14      /* control type: textMsg, callback function: (none) */


     /* Control Arrays: */

          /* (no control arrays in the resource file) */


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnCaptureSequence(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
