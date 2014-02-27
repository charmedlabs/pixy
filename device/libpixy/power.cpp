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

#include <math.h>
#include "pixy_init.h"
#include "misc.h"
#include "power.h"

static const ProcModule g_module[] =
{
	{
	"pwr_getVin",
	(ProcPtr)pwr_getVin, 
	{END}, 
	"Get Vin (JP1) voltage"
	"@r voltage in millivolts"
	},
	{
	"pwr_get5V",
	(ProcPtr)pwr_get5v, 
	{END}, 
	"Get 5V voltage"
	"@r voltage in millivolts"
	},
	{
	"pwr_getVbus",
	(ProcPtr)pwr_getVbus, 
	{END}, 
	"Get USB VBUS voltage"
	"@r voltage in millivolts"
	},
	{
	"pwr_USBpowered",
	(ProcPtr)pwr_USBpowered, 
	{END}, 
	"Determine if camera power is from USB host."
	"@r 0 if power is from Vin (JP1), nonzero if power is from USB"
	},
	END
};	

uint32_t pwr_getVin()
{
 	uint32_t vin;
	
	vin = adc_get(VIN_ADCCHAN)*10560/1024 + 330;  // 10560 = 3.3*3.2*1000, 330 is diode drop 
	
	return vin; 
}

uint32_t pwr_get5v()
{
	uint32_t v5;
	
	v5 = adc_get(V5_ADCCHAN)*5293/1024;  // 5293=3.3*1.604*1000 
	
	return v5; 
}

uint32_t pwr_getVbus()
{
	uint32_t vbus;
	
	vbus = adc_get(VBUS_ADCCHAN)*5293/1024;  // 5293=3.3*1.604*1000 
	
	return vbus; 
}

uint32_t pwr_USBpowered()
{
	if (LPC_GPIO_PORT->PIN[5]&0x0100)
		return 1;
	else
		return 0;
}

void pwr_init()
{
 	LPC_GPIO_PORT->DIR[5] |= 0x0100;	 

	// choose USB power or vin
	if (pwr_getVin()>6430) // 6430=5000+1100(ldo)+330(diode) 
 		LPC_GPIO_PORT->PIN[5] &= ~0x0100;
	else // switch usb on
		LPC_GPIO_PORT->PIN[5] |= 0x0100;

   	g_chirpUsb->registerModule(g_module);
}
