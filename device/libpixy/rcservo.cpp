#include "pixy_init.h"
#include "rcservo.h"

static uint16_t g_rcsPos[2];

static const ProcModule g_module[] =
{
	{
	"rcs_setPos",
	(ProcPtr)rcs_setPos, 
	{CRP_INT8, CRP_INT16, END}, 
	"Set RC-servo position"
	"@p channel value either 0 or 1 (2 possible channels)"
	"@p pos position value ranging from 0-999"
	"@r 0 if success, negative if error"
	},
	{
	"rcs_getPos",
	(ProcPtr)rcs_getPos, 
	{CRP_INT8, END}, 
	"Get RC-servo position"
	"@p channel value either 0 or 1 (2 possible channels)"
	"@r position value of the channel ranging from 0-999, negative if error"
	},
	{
	"rcs_enable",
	(ProcPtr)rcs_enable, 
	{CRP_INT8, CRP_INT16, END}, 
	"Enable/disable RC-servo"
	"@p channel value either 0 or 1 (2 possible channels)"
	"@p enable 0=disable, nonzero=enable"
	"@r 0 if success, negative if error"
	},
	END
};	


void rcs_init()
{
	rcs_setPos(0, RCS_MAX_POS/2);
	rcs_setPos(1, RCS_MAX_POS/2);
		
	g_chirpUsb->registerModule(g_module);
}

int32_t rcs_setPos(const uint8_t &channel, const uint16_t &pos, Chirp *chirp)
{
	if (channel>1 || pos>RCS_MAX_POS)
		return -1;

	LPC_SCT->MATCH[channel+1].L = RCS_MIN_PWM + pos;
	LPC_SCT->MATCHREL[channel+1].L = RCS_MIN_PWM + pos;
	LPC_SCT->OUT[channel+6].SET = 1<<0; 
	LPC_SCT->OUT[channel+6].CLR = 1<<(channel+1);

	g_rcsPos[channel] = pos;

	return 0;
}

int32_t rcs_getPos(const uint8_t &channel, Chirp *chirp)
{
	if (channel>1)
		return -1;

	return g_rcsPos[channel];	
}

int32_t rcs_enable(const uint8_t &channel, const uint8_t &enable, Chirp *chirp)
{
	if (channel>1)
		return -1;

	if (enable)
	{
		LPC_SCT->OUT[channel+6].SET = 1<<0; 
		LPC_SCT->OUT[channel+6].CLR = 1<<(channel+1);
	}
	else
	{
		LPC_SCT->OUT[channel+6].SET = 1<<15; // disable
		LPC_SCT->OUT[channel+6].CLR = 1<<0;
	}

	return 0;
}
