#ifndef L3_BANKUTILS_H
#define L3_BANKUTILS_H

#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include "daqFormats.h"

void *offlen2ptr(void *base, struct offlen ol);
void *offlen2ptr(void *base, struct offlen ol, char *type);

void *validate(void * ptr, char *type);
bool validateBank(void * ptr, char *type);

// Get the size of a bank in bytes (NOT dwords);
int bankSize(bankHeader bh);

#endif
