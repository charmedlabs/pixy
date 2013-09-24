#include <debug.h>
#include "chirpusb.h"
 
ChirpUsb::ChirpUsb() 
{
	setLink(&m_link);
}

ChirpUsb::~ChirpUsb()
{
}

int ChirpUsb::init()
{
	// don't call remoteInit because we come up first (and we're the USB slave)
	return 0;
}

