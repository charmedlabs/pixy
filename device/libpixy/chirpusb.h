#ifndef CHIRPUSB_H
#define CHIRPUSB_H

#include <chirp.hpp>
#include <usblink.h>

class ChirpUsb : public Chirp
{
public:
	ChirpUsb();
	~ChirpUsb();
	virtual int init();

private:
	USBLink m_link;
};

#endif
