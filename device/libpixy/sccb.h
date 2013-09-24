#ifndef _SCCB_H
#define _SCCB_H

#include "lpc43xx.h"

#define SCCB_DELAY		  100

#define CLK_MASK          (1<<1)
#define DATA_MASK         (1<<0)
#define DIR_REG 		  LPC_GPIO_PORT->DIR[0]
#define DATA_REG  		  LPC_GPIO_PORT->PIN[0]

class CSccb
	{
public:
	CSccb(unsigned char dev);
	void Write(unsigned char addr, unsigned char val);
	unsigned char Read(unsigned char addr);
	void Reset();

private:
	void Write(unsigned char val);
	inline void PortWrite(unsigned char clk, unsigned char data)
		{
		volatile unsigned long d;
		unsigned long bits = DATA_REG;
		if (clk)
			bits |= CLK_MASK;
		else
			bits &= ~CLK_MASK;
		if(data)
			bits |= DATA_MASK;
		else		 
			bits &= ~DATA_MASK;
		
		DATA_REG = bits; 	
		for (d=0; d<SCCB_DELAY; d++);
		}

	inline void Drive()
		{
		volatile unsigned long d;
		DIR_REG |= DATA_MASK;
		for (d=0; d<SCCB_DELAY; d++);
		}

	inline void TriState()
		{
		volatile unsigned long d;
		DIR_REG &= ~DATA_MASK;
		for (d=0; d<SCCB_DELAY; d++);
		}

	inline void Start()
		{
		PortWrite(1, 1);	// data, clk high
		Drive();	// take data out of tristate
		PortWrite(1, 0);	// data low
		PortWrite(0, 0);	// data low
		}

	inline void Stop()
		{
		PortWrite(1, 0);	// clk high, data low
		PortWrite(1, 1);	// clk, data high
		TriState();	// put data into tristate
		}

	unsigned char m_dev;
	unsigned short m_drive;
	};

#endif
