/*
 * @brief SPIFI Flash configuration header file
 *
 * @note
 * Copyright(C) NXP Semiconductors, 2012
 * All rights reserved.
 *
 * @par
 * Software that is described herein is for illustrative purposes only
 * which provides customers with programming information regarding the
 * LPC products.  This software is supplied "AS IS" without any warranties of
 * any kind, and NXP Semiconductors and its licensor disclaim any and
 * all warranties, express or implied, including all implied warranties of
 * merchantability, fitness for a particular purpose and non-infringement of
 * intellectual property rights.  NXP Semiconductors assumes no responsibility
 * or liability for the use of the software, conveys no license or rights under any
 * patent, copyright, mask work right, or any other intellectual property rights in
 * or to any products. NXP Semiconductors reserves the right to make changes
 * in the software without notification. NXP Semiconductors also makes no
 * representation or warranty that such application will be suitable for the
 * specified use without further testing or modification.
 *
 * @par
 * Permission to use, copy, modify, and distribute this software and its
 * documentation is hereby granted, under NXP Semiconductors' and its
 * licensor's relevant copyrights in the software, without fee, provided that it
 * is used in conjunction with NXP Semiconductors microcontrollers.  This
 * copyright, permission, and disclaimer notice must appear in all copies of
 * this code.
 */
 
#ifndef __FLASH_CONFIG_H_
#define __FLASH_CONFIG_H_

/** @ingroup EXAMPLES_MISC_18XX43XX_SPIFI_PROGRAMMER
 * @{
 */

/** @def USE_SPIFI_LIB
 * Macro that enables calling of spifi functions using the library,
 * disabling this will directly call the functions in boot rom using
 * bootROM function pointers.
 */
#define USE_SPIFI_LIB

//#include "board.h"
#include "spifi_rom_api.h"

// For LPC 1830/4330 nXplorer board
#define SPIFLASH_DEVICE_SIZE        (4 * 1024 * 1024)
#define SPIFLASH_SECTOR_SIZE        (64 * 1024)
#define SPIFLASH_NUM_OF_SECTORS     (SPIFLASH_DEVICE_SIZE/SPIFLASH_SECTOR_SIZE)
#define SPIFLASH_BASE_ADDRESS       0x14000000


/** @def SPIFI_WRITE_SECTOR_OFFSET
 * Offset of the sector to which the data to be written
 */
#define SPIFI_WRITE_SECTOR_OFFSET       0x40000
#define SPIFI_WRITE_SECTOR_ADDRESS      (SPIFLASH_BASE_ADDRESS + SPIFI_WRITE_SECTOR_OFFSET)

#define spifi_rom_init(x)

#ifndef USE_SPIFI_LIB
#define spifi_init                       spifi->spifi_init
#define spifi_program                    spifi->spifi_program
#undef  spifi_rom_init
#define spifi_rom_init(x)                define_spifi_romPtr(x)
#endif

/**
 * @}
 */

#endif /* ifndef __FLASH_CONFIG_H_ */
