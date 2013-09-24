#include "chirpm0.h"

ChirpM0::ChirpM0()
{
	setLink(&m_link);
}

ChirpM0::~ChirpM0()
{
}

int ChirpM0::init()
{
	return remoteInit();
}
