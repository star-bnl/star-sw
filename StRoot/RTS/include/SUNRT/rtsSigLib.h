#ifndef _SIGLIB_H_
#define _SIGLIB_H_

#include <signal.h>

void rtsCatchSignals(void (*)(int, siginfo_t *, void *));

#endif
