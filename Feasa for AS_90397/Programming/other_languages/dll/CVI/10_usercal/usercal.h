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
#define  PANEL_BTNREADPARAMS              2       /* control type: command, callback function: btnAdjust */
#define  PANEL_BTNADJUSTRGB               3       /* control type: command, callback function: btnAdjust */
#define  PANEL_BTNADJUSTXY                4       /* control type: command, callback function: btnAdjust */
#define  PANEL_BTNADJUSTWAVELENGTH        5       /* control type: command, callback function: btnAdjust */
#define  PANEL_BTNADJUSTABSINT            6       /* control type: command, callback function: btnAdjust */
#define  PANEL_BTNBALANCEINT              7       /* control type: command, callback function: btnAdjust */
#define  PANEL_DECORATION                 8       /* control type: deco, callback function: (none) */
#define  PANEL_TEXTMSG_4                  9       /* control type: textMsg, callback function: (none) */
#define  PANEL_LSTCOMMPORT                10      /* control type: ring, callback function: (none) */
#define  PANEL_NUMFIBERS                  11      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFXB                   12      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFXG                   13      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFABSINTB              14      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFYB                   15      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFXR                   16      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFABSINTG              17      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFYG                   18      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFABSINT               19      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFABSINTR              20      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFYR                   21      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFX                    22      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFWAVELENGTH           23      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTREFY                    24      /* control type: numeric, callback function: (none) */
#define  PANEL_TXTLOG                     25      /* control type: textBox, callback function: (none) */
#define  PANEL_LSTCAPTURE                 26      /* control type: ring, callback function: (none) */
#define  PANEL_CHKPWM                     27      /* control type: radioButton, callback function: (none) */
#define  PANEL_OPTRAMFLASH                28      /* control type: tree, callback function: (none) */


     /* Control Arrays: */

          /* (no control arrays in the resource file) */


     /* Menu Bars, Menus, and Menu Items: */

          /* (no menu bars in the resource file) */


     /* Callback Prototypes: */

int  CVICALLBACK btnAdjust(int panel, int control, int event, void *callbackData, int eventData1, int eventData2);
int  CVICALLBACK panelCB(int panel, int event, void *callbackData, int eventData1, int eventData2);


#ifdef __cplusplus
    }
#endif
