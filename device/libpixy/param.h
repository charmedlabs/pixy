#ifndef _PARAM_H
#define _PARAM_H

#include <stdarg.h>
#include "chirp.hpp"

int prm_init(Chirp *chirp);

int32_t prm_set(const char *id, ...);
int32_t prm_setChirp(const char *id, const uint32_t &valLen, const uint8_t *val);
int32_t prm_get(const char *id, ...);
int32_t prm_getChirp(const char *id, Chirp *chirp);
int32_t  prm_getInfo(const char *id, Chirp *chirp);
int32_t  prm_getAll(const uint16_t &index, Chirp *chirp);

int prm_add(const char *id, const char *desc, ...);
bool prm_verifyAll();
int prm_format();

#endif
