#ifndef _UTIL_H_
#define _UTIL_H_

struct rccnf
{
  int run;
  UINT32 detMask;
  UINT32 grpMask;
};

UINT32 str2detmask(const char *str);
UINT32 str2evpgroupmask(const char *str);
int getRccnf(const char *fn, rccnf *desc);

#endif
