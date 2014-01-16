#ifndef _BUTTON_H
#define _BUTTON_H

#include <inttypes.h>

class ButtonMachine
{
public:
	ButtonMachine();
	~ButtonMachine();

	bool handleSignature();

private:
	void reset();
	void flashLED(uint8_t flashes);
	void setLED();
	void ledPipe();
	void setSignature();
	void wait(uint32_t us);

	uint8_t m_goto;
	uint8_t m_index;
	uint32_t m_timer;
	bool m_ledPipe;
};

#endif
