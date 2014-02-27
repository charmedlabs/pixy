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

#ifndef _FLASHPROG_H
#define _FLASHPROG_H

#define VIN_ADCCHAN   	4
#define V5_ADCCHAN	  	3
#define VBUS_ADCCHAN	5

void flash_init();

uint32_t flash_sectorSize();
int32_t flash_program2(const uint32_t &addr, const uint32_t &len, const uint8_t *data);
int32_t flash_reset();

#endif
