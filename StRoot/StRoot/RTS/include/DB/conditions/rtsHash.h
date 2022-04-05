#ifndef _RTSHASH_H_
#define _RTSHASH_H_

#include "rtsDbConstants.h"

struct rtsHash
{
  int version;
  unsigned int hash;
  char comment[VARCHAR_LENGTH];
};

#endif
