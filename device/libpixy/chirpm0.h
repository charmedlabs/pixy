#ifndef CHIRPM0_H
#define CHIRPM0_H

#include <chirp.hpp>
#include <smlink.hpp>

class ChirpM0 : public Chirp
{
public:
	ChirpM0();
	~ChirpM0();
	virtual int init();

private:
	SMLink m_link;
};


#endif
