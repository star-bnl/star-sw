#ifndef _SIGLIB_H_
#define _SIGLIB_H_

#include <signal.h>

static void daqCatchSignals(void ()(int, siginfo_t *, void *));

#endif
