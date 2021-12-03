#ifndef RC_SYSSTATE_HH
#define RC_SYSSTATE_HH

#include "RC_Config.h"

struct SysTaskState
{
  uint16_t node;
  uint8_t task;
  uint8_t inrun;
  char name[MAX_STR_LEN];
  int state;
};

struct SysState
{
  SysTaskState states[MAX_NODES];
  int gState;
};


struct RtsErrorMsg
{
  uint16_t node;
  uint8_t task;
  uint8_t dummy;
  char msg[256];
};

#ifdef RTS_LITTLE_ENDIAN
inline void swapSysState(SysState *s)
{
  s->gState = ntohl(s->gState);
  for(int i=0;i<MAX_NODES;i++) {
    s->states[i].node = ntohs(s->states[i].node);
    s->states[i].state = ntohl(s->states[i].state);
  }
}

inline void swapRtsErrorMessage(RtsErrorMsg *s)
{
  s->node = ntohs(s->node);
}
#endif

#endif
