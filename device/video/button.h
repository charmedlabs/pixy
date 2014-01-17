#ifndef _BUTTON_H
#define _BUTTON_H

#include <inttypes.h>

#define BT_INITIAL_BUTTON_TIMEOUT     	1250*1000
#define BT_INDEX_CYCLE_TIMEOUT    		1000*1000
#define BT_LIGHTPIPE_TIMEOUT    		60000*1000
#define BT_FLASH_TIMEOUT 				60*1000
#define BT_PROG_TIMEOUT 				2000*1000

class ButtonMachine
{
public:
	ButtonMachine();
	~ButtonMachine();

	bool handleSignature();
	int selectProgram(int progs);

private:
	void reset();
	void wait(uint32_t us);
	void flashLED(uint8_t flashes);
	void setLED();
	void ledPipe();

	void setSignature();

	uint8_t m_goto;
	uint8_t m_index;
	uint32_t m_timer;
	bool m_ledPipe;
};

#endif
