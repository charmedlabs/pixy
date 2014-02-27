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

#include "sccb.h"

//CSccb g_sccb(0, 0);

CSccb::CSccb(unsigned char dev)
	{
	m_dev = dev;
	DIR_REG |= CLK_MASK;

	TriState();	// set clock as output, data tristate
	PortWrite(1, 0);	// set clock high (idle)	 


	//
#if 0
	volatile unsigned long val;
	Drive();
	PortWrite(0, 0);
	PortWrite(0, 1);
	PortWrite(1, 0);
	PortWrite(1, 1);
	TriState();	// set clock as output, data tristate
	val = DATA_REG&DATA_MASK;
	val = DATA_REG&DATA_MASK;
	val = DATA_REG&DATA_MASK;
#endif
	}

void CSccb::Reset()
	{
	volatile unsigned long d; 

	Read(0x00);
	Write(0x12, 0xa4); // reset operation
	for (d=0; d<1000; d++);
	Read(0x00);
	}

void CSccb::Write(unsigned char addr, unsigned char val)
	{
	Start();
	Write(m_dev);
	Write(addr);
	Write(val);
	Stop();
	}

unsigned char CSccb::Read(unsigned char addr)
	{
	unsigned char i, val;

	// 2 phase
	Start();
	Write(m_dev);
	Write(addr);
	Stop();

	Start();
	Write(m_dev + 1);
	TriState();
	for (i=0, val=0; i<8; i++)
		{
		val <<= 1;
		PortWrite(1, 0);
		if (DATA_REG&DATA_MASK)
			val |= 1;
		PortWrite(0, 0);
		}

	// send NA bit
	PortWrite(0, 1);
	Drive();
	PortWrite(1, 1);
	PortWrite(0, 1);

	Stop();

	return val;
	}

void CSccb::Write(unsigned char val)
	{
	unsigned char i;

	for (i=0; i<8; i++, val<<=1)
		{
		if (val&0x80) // send msb first
			{
			PortWrite(0, 1);
			PortWrite(1, 1);
			PortWrite(0, 1);
			}
		else
			{
			PortWrite(0, 0);
			PortWrite(1, 0);
			PortWrite(0, 0);
			}
		}
	// send "don't care" bit
	TriState();
	PortWrite(0, 0);
	PortWrite(1, 0);
	PortWrite(0, 0);
	Drive();
	}

