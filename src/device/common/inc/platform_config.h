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

#ifndef __PLATFORM_CONFIG_H 
#define __PLATFORM_CONFIG_H

#include "stdint.h"

#ifdef __cplusplus
 extern "C" {
#endif 

/****************************************************/
/* supported platforms list							*/
/* DO NOT CHANGE THESE DEFINITIONS 					*/
#define NXP_VALIDATION_BOARD	(1)
#define HITEX_A2_BOARD          (3)
/****************************************************/

/****************************************************/
/* supported devices list							*/
/* DO NOT CHANGE THESE DEFINITIONS 					*/
#define LPC43xx	(1)
/****************************************************/

/****************************************************/
/* used for the configuration checks */
/* DO NOT CHANGE THESE DEFINITIONS 					*/
/****************************************************/
#define YES	(1)
#define NO	(2)

/****************************************************/
/* USER CONFIGURATION SECTION						*/
/****************************************************/

/* choose the platform you want to build against 	*/
#define PLATFORM HITEX_A2_BOARD

/* choose the device you want to build against 	*/
#define DEVICE	LPC43xx

/* these definitions are being taken from the project build rule */
#ifdef EXT_FLASH
#define	USE_EXT_FLASH	(YES)
#else 
#define USE_EXT_FLASH	(NO)
#endif

#ifdef EXT_STAT_MEM
#define USE_EXT_STATIC_MEM	(YES)
#else
#define USE_EXT_STATIC_MEM	(NO)
#endif

#ifdef EXT_DYN_MEM
#define USE_EXT_DYNAMIC_MEM	(YES)
#else
#define USE_EXT_DYNAMIC_MEM	(NO)
#endif


/* specify the filename used for the slave image */
#define SLAVE_IMAGE_FILE "CM0_image.c"


/* configure which priority the mailbox interrupt should have on the M4 side */
/* cmsis definition, priority from 0 to 7 */
#define MASTER_MAILBOX_PRIORITY	(0)

/* configure which priority the mailbox interrupt should have on the M0 side */
/* cmsis definition, priority from 0 to 3 */
#define SLAVE_MAILBOX_PRIORITY	(0)

/* memory map for the application */
/* !!! needs to be consistent with the scatter file !!! */
#ifdef EXT_FLASH

/************************************/
/* this is for the FLASH version 	*/
/************************************/
/*	0x1C000000	M4 ROM 4Mbytes		*/
/*	0x1C3FFFFF						*/
/*	0x10000000	M4 RAM 96K			*/
/*	0x10017FFF						*/
#define MASTER_ROM_START	0x1C000000
#define MASTER_ROM_LEN		0x400000	/* 4 Mbytes */

#define MASTER_RAM_START	0x10000000	/* 96 Kbytes */
#define MASTER_RAM_LEN		0x18000

/*	0x10080000	M0 ROM 32K	*/
/*	0x10087FFF				*/
/*	0x10088000 	M0 RAM 8K	*/
/*	0x10089FFF 				*/
#define SLAVE_ROM_START	0x10080000
#define SLAVE_ROM_LEN		0x8000

#define SLAVE_RAM_START	 	0x2000c000
#define SLAVE_RAM_LEN		0x4000

/*	0x20000000  M4 BUF 16K	*/
/*	0x20003FFF				*/
/*	0x20004000	M0 BUF	16K	*/
/*	0x20007FFF				*/
#define MASTER_BUF_START	0x20000000
#define MASTER_BUF_LEN		0x4000

#define SLAVE_BUF_START		0x20004000
#define SLAVE_BUF_LEN		0x4000

/*	0x20008000	M4 MBX 8K	*/
/*	0x20009FFF				*/
/*	0x2000A000	M0 MBX 8K	*/
/*	0x2000BFFF				*/
#define MASTER_MBX_START	0x20008000
#define MASTER_MBX_LEN		0x2000

#define SLAVE_MBX_START	0x2000A000
#define SLAVE_MBX_LEN		0x2000

#else 

/*******************************/
/* this is for the ram version */
/*******************************/
/*	0x10000000	M4 ROM 64K	*/
/*	0x1000FFFF				*/
/*	0x10010000	M4 RAM 32K	*/
/*	0x10017FFF				*/
#define MASTER_ROM_START	0x10000000
#define MASTER_ROM_LEN		0x10000

#define MASTER_RAM_START	0x10010000
#define MASTER_RAM_LEN		0x8000

/*	0x10080000	M0 ROM 32K	*/
/*	0x10087FFF				*/
/*	0x10088000 	M0 RAM 8K	*/
/*	0x10089FFF 				*/
#define SLAVE_ROM_START	0x10080000
#define SLAVE_ROM_LEN		0x8000

#define SLAVE_RAM_START		0x2000c000
#define SLAVE_RAM_LEN		0x4000

/*	0x20000000  M4 BUF 16K	*/
/*	0x20003FFF				*/
/*	0x20004000	M0 BUF	16K	*/
/*	0x20007FFF				*/
#define MASTER_BUF_START	0x20000000
#define MASTER_BUF_LEN		0x4000

#define SLAVE_BUF_START		0x20004000
#define SLAVE_BUF_LEN		0x4000

/*	0x20008000	M4 MBX 8K	*/
/*	0x20009FFF				*/
/*	0x2000A000	M0 MBX 8K	*/
/*	0x2000BFFF				*/
#define MASTER_MBX_START	0x20008000
#define MASTER_MBX_LEN		0x2000

#define SLAVE_MBX_START		0x2000A000
#define SLAVE_MBX_LEN		0x2000

#endif /* ifdef EXT_FLASH */

/****************************************************/
/* END OF USER CONFIGURATION 						*/
/* DO NOT EDIT BELOW THIS LINE						*/
/****************************************************/

/* assign the roles for the devices */
#if (DEVICE==LPC43xx)

#include "lpc43xx.h"

#define MASTER_CPU CORE_M4
#define SLAVE_CPU CORE_M0

#define MASTER_IRQn (1)
#define SLAVE_IRQn 	(1)

#define MASTER_TXEV_FLAG 	((uint32_t *) 0x40043130)
#define MASTER_TXEV_QUIT() 	{ *MASTER_TXEV_FLAG = 0x0; }

#define SLAVE_TXEV_FLAG ((uint32_t *) 0x40043400)
#define SLAVE_TXEV_QUIT() { *SLAVE_TXEV_FLAG = 0x0; }

#define SLAVE_SHADOW_REG	0x40043404

#endif


#define MASTER_IPC_TABLE	MASTER_MBX_START
#define SLAVE_IPC_TABLE		SLAVE_MBX_START

/****************************************************/
/* platform wise initialization functions			*/
/****************************************************/
void platformInit(void);

#ifdef __cplusplus
}
#endif 
 
#endif /* __PLATFORM_CONFIG_H */

