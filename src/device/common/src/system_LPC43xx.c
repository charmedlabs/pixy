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
* $Id$		system_lpc43xx.c			2011-06-02
*//**
* @file		system_lpc43xx.c
* @brief	Cortex-M3 Device System Source File for NXP lpc43xx Series.
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

#include "lpc43xx.h"
#include "lpc43xx_cgu.h"


#ifdef KEIL
extern uint32_t getPC(void);
#endif

/**
 * Initialize the system
 *
 * @param  none
 * @return none
 *
 * @brief  Setup the microcontroller system.
 *         Initialize the System.
 */
void SystemInit (void)
{
#ifdef KEIL

#if defined(CORE_M4) || defined(CORE_M3)
	// Enable VTOR register to point to vector table
	SCB->VTOR = getPC() & 0xFFF00000;

#else // code red
    // CodeRed startup code will modify VTOR register to match
    // when code has been linked to run from.

    // Check whether we are running from external flash
    if (SCB->VTOR == 0x1C000000)
        /*Enable Buffer for External Flash*/
        LPC_EMC->STATICCONFIG0 |= 1<<19;

    // Call clock initialisation code
    CGU_Init();

#endif

    /*Enable Buffer for External Flash*/
    LPC_EMC->STATICCONFIG0 |= 1<<19;
#endif
}
