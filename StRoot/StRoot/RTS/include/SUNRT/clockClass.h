#ifndef _CLOCKCLASS_H_
#define _CLOCKCLASS_H_

// Only for linux
// Only for use from ROOT

#include <unistd.h>
#include <sys/time.h>

class RtsTimer_root
{
 public:

  timeval ts_old;
  timeval ts_last;
  timeval ts_new;

  double t;

  RtsTimer_root() {
    reset();
  }

  void reset() {
    gettimeofday(&ts_old,NULL);
    ts_last = ts_old;
  }

  double currtime() {
    gettimeofday(&ts_new, NULL);
    t = ts_new.tv_sec - ts_old.tv_sec;
    t += ((double)(ts_new.tv_usec - ts_old.tv_usec))/1000000.0;

    return t;
  }
  
  double record_time() {
    gettimeofday(&ts_new, NULL);
    t = ts_new.tv_sec - ts_last.tv_sec;
    t += ((double)(ts_new.tv_usec - ts_last.tv_usec))/1000000.0;
    ts_last = ts_new;
    return t;
  } 
};

#endif
