//*****************************************************************************
//   +--+
//   | ++----+
//   +-++    |
//     |     |
//   +-+--+  |
//   | +--+--+
//   +----+    Copyright (c) 2013 Code Red Technologies Ltd.
//
// cr_start_m0.c
//
// Provides function for CM4 'master' CPU in an NXP LPC43xx MCU to release
// CM0 'slave' CPUs from reset and begin executing.
//
// Version : 130410
//
// Software License Agreement
//
// The software is owned by Code Red Technologies and/or its suppliers, and is
// protected under applicable copyright laws.  All rights are reserved.  Any
// use in violation of the foregoing restrictions may subject the user to criminal
// sanctions under applicable laws, as well as to civil liability for the breach
// of the terms and conditions of this license.
//
// THIS SOFTWARE IS PROVIDED "AS IS".  NO WARRANTIES, WHETHER EXPRESS, IMPLIED
// OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE APPLY TO THIS SOFTWARE.
// USE OF THIS SOFTWARE FOR COMMERCIAL DEVELOPMENT AND/OR EDUCATION IS SUBJECT
// TO A CURRENT END USER LICENSE AGREEMENT (COMMERCIAL OR EDUCATIONAL) WITH
// CODE RED TECHNOLOGIES LTD.
//
//*****************************************************************************

#include "cr_start_m0.h"

// Provide defines for accessing peripheral registers necessary to release
// CM0 slave processors from reset. Note that this code does not use the
// CMSIS register access mechanism, as there is no guarantee that the
// project has been configured to use CMSIS.
#define RGU_RESET_CTRL1	          (*((volatile uint32_t *) 0x40053104))
#define RGU_RESET_ACTIVE_STATUS1  (*((volatile uint32_t *) 0x40053154))
#define RGU_RESET_CTRL0	          (*((volatile uint32_t *) 0x40053100))
#define RGU_RESET_ACTIVE_STATUS0  (*((volatile uint32_t *) 0x40053150))
#define CREG_M0APPMEMMAP	        (*((volatile uint32_t *) 0x40043404))
#define CREG_M0SUBMEMMAP          (*((volatile uint32_t *) 0x40043308))

/*******************************************************************
 * Static function to Release SLAVE processor from reset
 *******************************************************************/
static void startSlave(uint32_t slavenum) {

	volatile uint32_t u32REG, u32Val;

	if (slavenum <= SLAVE_M0SUB) {

		if (slavenum == SLAVE_M0APP) {
			/* Release Slave from reset, first read status */
			/* Notice, this is a read only register !!! */
			u32REG = RGU_RESET_ACTIVE_STATUS1;

			/* If the M0 is being held in reset, release it */
			/* 1 = no reset, 0 = reset */
			while (!(u32REG & (1u << 24))) {
				u32Val = (~(u32REG) & (~(1 << 24)));
				RGU_RESET_CTRL1 = u32Val;
				u32REG = RGU_RESET_ACTIVE_STATUS1;
			};
		}
		else { // (slavenum == SLAVE_M0SUB)
			/* Release Slave from reset, first read status */
			/* Notice, this is a read only register !!! */
			u32REG = RGU_RESET_ACTIVE_STATUS0;

			/* If the M0 is being held in reset, release it */
			/* 1 = no reset, 0 = reset */
			while (!(u32REG & (1u << 12))) {
				u32Val = (~(u32REG) & (~(1 << 12)));
				RGU_RESET_CTRL0 = u32Val;
				u32REG = RGU_RESET_ACTIVE_STATUS0;
			};

		}
	}
}

/*******************************************************************
 * Static function to put the SLAVE processor back in reset
 *******************************************************************/
static void haltSlave(uint32_t slavenum) {

	volatile uint32_t u32REG, u32Val;

	if (slavenum <= SLAVE_M0SUB) {

		if (slavenum == SLAVE_M0APP) {

			/* Check if M0 is reset by reading status */
			u32REG = RGU_RESET_ACTIVE_STATUS1;

			/* If the M0 has reset not asserted, halt it... */
			/* in u32REG, status register, 1 = no reset */
			while ((u32REG & (1u << 24))) {
				u32Val = ((~u32REG) | (1 << 24));
				RGU_RESET_CTRL1 = u32Val;
				u32REG = RGU_RESET_ACTIVE_STATUS1;
			}
		} else { // (slavenum == SLAVE_M0SUB)
			/* Check if M0 is reset by reading status */
			u32REG = RGU_RESET_ACTIVE_STATUS0;

			/* If the M0 has reset not asserted, halt it... */
			/* in u32REG, status register, 1 = no reset */
			while ((u32REG & (1u << 12))) {
				u32Val = ((~u32REG) | (1 << 12));
				RGU_RESET_CTRL0 = u32Val;
				u32REG = RGU_RESET_ACTIVE_STATUS0;
			}

		}
	}

}

/*******************************************************************
 * Function to start required CM0 slave cpu executing
 *******************************************************************/
void cr_start_m0(uint32_t slavenum, uint8_t *CM0image_start)  {

	if (slavenum <= SLAVE_M0SUB) {

		// Make sure M0 is not running
		haltSlave(slavenum);

		// Set M0's vector table to point to start of M0 image
		if (slavenum == SLAVE_M0APP) {
			CREG_M0APPMEMMAP = (uint32_t) CM0image_start;
		} else { // (slavenum == SLAVE_M0SUB)
			CREG_M0SUBMEMMAP = (uint32_t) CM0image_start;
		}
		// Release M0 from reset
		startSlave(slavenum);
	}
}
