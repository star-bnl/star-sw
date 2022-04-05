#ifndef _RTSCNDLXUSERINT_H_
#define _RTSCNDLXUSERINT_H_

#include "rtsDbConstants.h"

struct rtsCndLxUserInt
{
  ////////////////
  int idx_rn;
  int idx_trigger;  // what trigger (1..32)
  int idx_level;    // level 1, level 2, or level 3 
  int idx_alg;      // what algorithm id
  int idx_reg;      // what dictionary register
  ////////////////

  int userInt;
};

#endif
