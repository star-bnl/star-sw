#ifndef __JPROF_H__
#define __JPROF_H__

/// class Jprof Dummy class to begin profiling with jprof.
///            
/// Before loading Jprof.so, set the environment variable JPROF_FLAGS
/// to one or more of the following values (space delimited):
/// 
/// JP_START: Install the signal handler, and start sending the timer signals. 
/// JP_DEFER: Install the signal handler, but don't start sending the timer 
///           signals. The user must start the signals by sending the first one
///           (with kill -PROF, or with kill -ALRM if JP_REALTIME is used). 
/// JP_FIRST=x: Wait x seconds before starting the timer 
/// JP_PERIOD=y: Set timer to interrupt every y seconds, y must be greater than
///              0.001 (1 ms).
/// JP_REALTIME: Do the profiling in intervals of real time rather than
///              intervals of time used by the process (and the kernel when 
///              doing work for the process). This could probably lead to 
///              weird results (you'll see whatever runs when the process is 
///              waiting for events), but is needed to see time spent in the 
///              X server, etc. 

#include "jprof.h"

class Jprof{ 
public:
  Jprof();
  virtual ~Jprof();
};

/// Instance created at library load - 
/// Make sure to set environment variable JPROF_FLAGS before loading library.
extern Jprof *gJprof; 

#endif /* __JPROF_H__ */
