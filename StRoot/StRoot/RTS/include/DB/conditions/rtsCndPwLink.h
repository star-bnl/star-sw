#ifndef _RTSCNDPWLINK_H_
#define _RTSCNDPWLINK_H_

#include "rtsDbConstants.h"

struct rtsCndPwLink
{
  //////////////////////////
  // int idx_rn;
  int idx_twhash;
  //////////////////////////

  unsigned int pw;
  unsigned int pwdef;
};

#endif
