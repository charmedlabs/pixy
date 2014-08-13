//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

/*----------------------------------------------------------------------------
 *      U S B  -  K e r n e l
 *----------------------------------------------------------------------------
 * Name:    usbuser.h
 * Purpose: USB Custom User Definitions
 * Version: V1.20
 *----------------------------------------------------------------------------
 *      This software is supplied "AS IS" without any warranties, express,
 *      implied or statutory, including but not limited to the implied
 *      warranties of fitness for purpose, satisfactory quality and
 *      noninfringement. Keil extends you a royalty-free right to reproduce
 *      and distribute executable files created using this software for use
 *      on NXP Semiconductors LPC family microcontroller devices only. Nothing 
 *      else gives you the right to use this software.
 *
 * Copyright (c) 2009 Keil - An ARM Company. All rights reserved.
 *---------------------------------------------------------------------------*/

#ifndef __USBUSER_H__
#define __USBUSER_H__

#ifdef __cplusplus
 extern "C" {
#endif

#define USB_MAX_CHUNK        0x4000

/* USB Device Events Callback Functions */
extern void USB_Power_Event     (uint32_t power);
extern void USB_Reset_Event     (void);
extern void USB_Suspend_Event   (void);
extern void USB_Resume_Event    (void);
extern void USB_WakeUp_Event    (void);
extern void USB_SOF_Event       (void);
extern void USB_Error_Event     (uint32_t error);

/* USB Endpoint Events Callback Pointers */
extern void (* const USB_P_EP[USB_EP_NUM])(uint32_t event);

/* USB Endpoint Events Callback Functions */
extern void USB_EndPoint0  (uint32_t event);
extern void USB_EndPoint1  (uint32_t event);
extern void USB_EndPoint2  (uint32_t event);
extern void USB_EndPoint3  (uint32_t event);
extern void USB_EndPoint4  (uint32_t event);
extern void USB_EndPoint5  (uint32_t event);

/* USB Core Events Callback Functions */
extern void USB_Configure_Event (void);
extern void USB_Interface_Event (void);
extern void USB_Feature_Event   (void);

extern void USB_UserInit(void);
extern uint32_t USB_handleState(void);
extern void USB_Recv(uint8_t *data, uint32_t len);
extern void USB_Send(const uint8_t *data, uint32_t len);
extern void USB_RecvReset(void);
extern void USB_SendReset(void);
extern volatile uint8_t g_sendComplete;
extern volatile uint8_t g_recvComplete;
extern volatile uint32_t g_sendTimerStart;
extern volatile uint32_t g_recvTimerStart;

#ifdef __cplusplus
 }
#endif

#endif  /* __USBUSER_H__ */
