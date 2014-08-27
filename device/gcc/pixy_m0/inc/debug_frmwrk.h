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

/**********************************************************************
* $Id$		debug_frmwrk.h			2011-06-02
*//**
* @file		debug_frmwrk.h
* @brief	Contains some utilities that used for debugging through UART
* @version	1.0
* @date		02. June. 2011
* @author	NXP MCU SW Application Team
*
* Copyright(C) 2011, NXP Semiconductor
* All rights reserved.
*
***********************************************************************
* Software that is described herein is for illustrative purposes only
* which provides customers with programming information regarding the
* products. This software is supplied "AS IS" without any warranties.
* NXP Semiconductors assumes no responsibility or liability for the
* use of the software, conveys no license or title under any patent,
* copyright, or mask work right to the product. NXP Semiconductors
* reserves the right to make changes in the software without
* notification. NXP Semiconductors also make no representation or
* warranty that such application will be suitable for the specified
* use without further testing or modification.
**********************************************************************/

/* Peripheral group ----------------------------------------------------------- */
/** @defgroup DEBUG_FRMWRK DEBUG FRAMEWORK
 * @ingroup LPC4300CMSIS_FwLib_Drivers
 * @{
 */

#ifndef DEBUG_FRMWRK_H_
#define DEBUG_FRMWRK_H_

/* Includes ------------------------------------------------------------------- */
#include "lpc43xx_uart.h"



#define USED_UART_DEBUG_PORT	1

#if (USED_UART_DEBUG_PORT==0)
#define DEBUG_UART_PORT	LPC_UART0
#elif (USED_UART_DEBUG_PORT==1)
#define DEBUG_UART_PORT	LPC_UART1
#endif

#define _DBG(x)	 	UARTPuts((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBG_(x)	UARTPuts_((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBC(x)	 	UARTPutChar((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBD(x)	 	UARTPutDec((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBD16(x)	UARTPutDec16((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBD32(x)	UARTPutDec32((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBH(x)	 	UARTPutHex((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBH16(x)	UARTPutHex16((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DBH32(x)	UARTPutHex32((LPC_USARTn_Type*)DEBUG_UART_PORT, x)
#define _DG			UARTGetChar((LPC_USARTn_Type*)DEBUG_UART_PORT)
#define _CR()       _DBG("\n")


#ifdef __cplusplus
extern "C"
{
#endif

void  lpc_printf (const  char *format, ...);


void UARTPutChar (LPC_USARTn_Type *UARTx, uint8_t ch);
void UARTPuts(LPC_USARTn_Type *UARTx, const void *str);
void UARTPuts_(LPC_USARTn_Type *UARTx, const void *str);
void UARTPutDec(LPC_USARTn_Type *UARTx, uint8_t decnum);
void UARTPutDec16(LPC_USARTn_Type *UARTx, uint16_t decnum);
void UARTPutDec32(LPC_USARTn_Type *UARTx, uint32_t decnum);
void UARTPutHex (LPC_USARTn_Type *UARTx, uint8_t hexnum);
void UARTPutHex16 (LPC_USARTn_Type *UARTx, uint16_t hexnum);
void UARTPutHex32 (LPC_USARTn_Type *UARTx, uint32_t hexnum);
uint8_t UARTGetChar (LPC_USARTn_Type *UARTx);
#define debug_frmwrk_init() debug_frmwrk_init_clk(0)
void debug_frmwrk_init_clk(uint32_t Clock_Speed);

#ifdef __cplusplus
}
#endif

#endif /* DEBUG_FRMWRK_H_ */

/**
 * @}
 */

/* --------------------------------- End Of File ------------------------------ */
