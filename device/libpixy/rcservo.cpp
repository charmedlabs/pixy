#include "pixy_init.h"
#include "rcservo.h"

static uint16_t g_rcsPos[RCS_NUM_AXES];
static int16_t g_rcsMinPwm[RCS_NUM_AXES];
static int16_t g_rcsPwmGain[RCS_NUM_AXES];

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
	{
	"rcs_setLimits",
	(ProcPtr)rcs_setLimits, 
	{CRP_INT8, CRP_INT16, CRP_INT16, END}, 
	"Set upper and lower limits of servo travel"
	"@p channel value either 0 or 1 (2 possible channels)"
	"@p lower value between -500 and 500. 0 is default, -500 will extend the lower limit the greatest amount."
	"@p upper value between -500 and 500. 0 is default, 500 will extend the upper limit the greatest amount."
	"@r 0 if success, negative if error"
	},
	{
	"rcs_setFreq",
	(ProcPtr)rcs_setFreq, 
	{CRP_INT16, END}, 
	"Set the PWM frequency"
	"@p frequency value between 20 and 300. 50 is default."
	"@r 0 if success, negative if error"
	},
	END
};	


void rcs_init()
{
	int i;

	for (i=0; i<RCS_NUM_AXES; i++)
	{
		g_rcsMinPwm[i] = RCS_MIN_PWM;
		g_rcsPwmGain[i] = 1<<RCS_GAIN_SCALE;
		rcs_setPos(i, RCS_CENTER_POS);
	}
		
	g_chirpUsb->registerModule(g_module);
}

int32_t rcs_setPos(const uint8_t &channel, const uint16_t &pos)
{
	uint16_t newPos;

	if (channel>=RCS_NUM_AXES || pos>RCS_MAX_POS)
		return -1;

	// scale position
	newPos = ((uint32_t)pos*g_rcsPwmGain[channel])>>RCS_GAIN_SCALE;

	LPC_SCT->MATCH[channel+1].L = g_rcsMinPwm[channel] + newPos;
	LPC_SCT->MATCHREL[channel+1].L = g_rcsMinPwm[channel] + newPos;
	LPC_SCT->OUT[channel+6].SET = 1<<0; 
	LPC_SCT->OUT[channel+6].CLR = 1<<(channel+1);

	g_rcsPos[channel] = pos;

	return 0;
}

int32_t rcs_getPos(const uint8_t &channel)
{
	if (channel>=RCS_NUM_AXES)
		return -1;

	return g_rcsPos[channel];	
}

int32_t rcs_enable(const uint8_t &channel, const uint8_t &enable)
{
	if (channel>=RCS_NUM_AXES)
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

int32_t rcs_setLimits(const uint8_t &channel, const int16_t &lower, const int16_t &upper)
{
	if (channel>=RCS_NUM_AXES || upper>500 || upper<-500 || lower>500 || lower<-500)
		return -1;

	g_rcsMinPwm[channel] = RCS_MIN_PWM+lower;
	// MAXPOS*gain = RANGE+upper-lower -> gain<<RCS_GAIN_SCALE = ((RANGE+upper-lower)<<RCS_GAIN_SCALE)/MAXPOS
	g_rcsPwmGain[channel] = ((RCS_PWM_RANGE+upper-lower)<<RCS_GAIN_SCALE)/RCS_MAX_POS;

	// update
	rcs_setPos(channel, g_rcsPos[channel]);

	return 0;
}

int32_t rcs_setFreq(const uint16_t &freq)
{
	uint16_t val;

	if (freq<20 || freq>300)
		return -1;

	val = RCS_CLOCK_FREQ/freq;

	LPC_SCT->MATCH[0].L = val; 
	LPC_SCT->MATCHREL[0].L = val;

	return 0;
}

