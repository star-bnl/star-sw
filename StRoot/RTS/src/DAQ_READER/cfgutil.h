#ifndef _UTIL_H_
#define _UTIL_H_

struct rccnf
{
  int run;
  UINT32 detMask;
  UINT32 grpMask;
};

UINT32 str2detmask(char *str);
UINT32 str2evpgroupmask(char *str);
int getRccnf(char *fn, rccnf *desc);

#endif
