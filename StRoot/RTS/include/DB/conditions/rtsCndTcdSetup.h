#ifndef _RTSCNDTCDSETUP_H_
#define _RTSCNDTCDSETUP_H_

// Added 10/04

#include "rtsDbConstants.h"

struct rtsCndTcdSetup
{
  ////////////
  unsigned int hash;          
  ////////////

  int tcdId;
  int phase;
  int gg_width;
  int daq_busy;
  int delay4;
  int delay8;
  int res1;
  int res2;
  int res3;
  int res4;
};

#endif
