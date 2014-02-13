#include "LPC43xx.h"
#include "lpc43xx_scu.h"
#include "misc.h"
#include "cameravals.h"
#include "analogdig.h"
#include "conncomp.h"

AnalogDig *g_ad = 0;

AnalogDig::AnalogDig()
{
	m_x = true;
	m_lastDetect = false;
}

void AnalogDig:: setDirection(bool x)
{
	m_x = x;
}

int AnalogDig::open()
{
	// set pin 1 on I/O connector to output	(Use SGPIO because GPIO interferes with pixel sync)
	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC2); //SGPIO10
	LPC_SGPIO->OUT_MUX_CFG10 = 4;
	LPC_SGPIO->GPIO_OENREG = 1<<10;
	LPC_DAC->CTRL =	(1<<3); // enable dac output

	return 0;	
}

int AnalogDig::close()
{
	LPC_DAC->CTRL = 0; // disable dac output	
	// set back to MOSI (input)
	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC5); // SSP1_MOSI	 

	return 0;	
}


int AnalogDig::update()
{
	uint32_t val;
	BlobA *blob;

	blob = (BlobA *)g_blobs->getMaxBlob();
	if (blob)
	{
		if (m_x)
		{
			val = blob->m_left+(blob->m_right-blob->m_left)/2;
			val *= (1<<16);
			val /= CAM_RES2_WIDTH;
		}
		else
		{
			val = CAM_RES2_HEIGHT-(blob->m_top+(blob->m_bottom-blob->m_top)/2);
			val *= (1<<16);
			val /= CAM_RES2_HEIGHT;
		}
		val &= 0x0000ffc0; // mask other bits (because datasheet instructs us to do so)

		// write dac val
		LPC_DAC->CR = val;

		// delay to let DAC settle before asserting pin 1
		if (m_lastDetect==false)
			delayus(100);
		// assert digital output
		LPC_SGPIO->GPIO_OUTREG = 1<<10;

	}
	else // negate digital output
	{
		LPC_SGPIO->GPIO_OUTREG = 0;
		// zero dac output
		LPC_DAC->CR = 0;
	}

	m_lastDetect = blob!=0;
	return 0;
}

void ad_init()
{
	g_ad = new AnalogDig;
}



