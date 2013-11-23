#ifndef CHIRPUSB_H
#define CHIRPUSB_H

#include <chirp.hpp>
#include <usblink.h>

class ChirpUsb : public Chirp
{
public:
	ChirpUsb();
	~ChirpUsb();

private:
	USBLink m_link;
};

#endif
