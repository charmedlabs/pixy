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

#ifndef _POWER_H
#define _POWER_H

#define VIN_ADCCHAN   	4
#define V5_ADCCHAN	  	3
#define VBUS_ADCCHAN	5

void pwr_init();

uint32_t pwr_getVin();
uint32_t pwr_get5v();
uint32_t pwr_getVbus();
uint32_t pwr_USBpowered();

#endif
