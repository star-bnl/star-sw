#ifndef _RTSCNDLXUSERFLOAT_H_
#define _RTSCNDLXUSERFLOAT_H_

#include "rtsDbConstants.h"

struct rtsCndLxUserFloat
{
  ////////////////
  int idx_rn;
  int idx_trigger;
  int idx_level;    // level 1, level 2, or level 3 
  int idx_alg;
  int idx_reg;    
  ////////////////

  float userFloat;
};

#endif
