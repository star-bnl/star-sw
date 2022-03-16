#ifndef _CLOCK_H_
#define _CLOCK_H_

#include <unistd.h>

#if defined(__linux__) || defined(__APPLE__)
#ifndef linux
#define linux
#endif
#endif

#ifdef linux
#include <sys/time.h>
#else
#include <time.h>
#endif

#if defined(i686) || defined(i386)
typedef unsigned long long RTS_CPUCYCLE;  // always 64 bits...
#include <I386/i386Lib.h>
#elif defined(vxworks)
typedef unsigned int RTS_CPUCYCLE;  // always 64 bits...
#include <MVME/mvmeFastTickerLib.h>
#else
typedef unsigned long long RTS_CPUCYCLE;
#endif

#ifdef vxworks
#include <taskLib.h>
#endif

class RtsTimer
{
 public:
#ifdef linux
  timeval ts_old;
  timeval ts_last;
  timeval ts_new;
#else
  timespec ts_old;
  timespec ts_last;
  timespec ts_new;
#endif
  double t;

  RtsTimer() {
    reset();
  }

  void reset() {
#ifdef linux
    gettimeofday(&ts_old,NULL);
#else
    clock_gettime(CLOCK_REALTIME, &ts_old);
#endif
    ts_last = ts_old;
  }

  double currtime() {
#ifdef linux
      gettimeofday(&ts_new, NULL);
#else
      clock_gettime(CLOCK_REALTIME, &ts_new);
#endif
      t = ts_new.tv_sec - ts_old.tv_sec;
      
#ifdef linux
      t += ((double)(ts_new.tv_usec - ts_old.tv_usec))/1000000.0;
#else
      t += ((double)(ts_new.tv_nsec - ts_old.tv_nsec))/1000000000.0;
#endif
      return t;
  } 


  double actualtime() {
#ifdef linux
    gettimeofday(&ts_new, NULL);
#else
    clock_gettime(CLOCK_REALTIME, &ts_new);
#endif
    t = ts_new.tv_sec;
    
#ifdef linux
    t += ((double)(ts_new.tv_usec))/1000000.0;
#else
    t += ((double)(ts_new.tv_nsec))/1000000000.0;
#endif
    return t;
  }
  
  double record_time() {
#ifdef linux
    gettimeofday(&ts_new, NULL);
#else
    clock_gettime(CLOCK_REALTIME, &ts_new);
#endif
    t = ts_new.tv_sec - ts_last.tv_sec;

#ifdef linux
    t += ((double)(ts_new.tv_usec - ts_last.tv_usec))/1000000.0;
#else
    t += ((double)(ts_new.tv_nsec - ts_last.tv_nsec))/1000000000.0;
#endif
    ts_last = ts_new;
    return t;
  }

  double stopwatch;
  int stopwatchcounting;

  void StopWatchReset() {
      stopwatchcounting = 0;
      stopwatch = 0;
  }

  double StopWatchStop() {
      if(stopwatchcounting) stopwatch += record_time();
      stopwatchcounting = 0;
      return stopwatch;
  }

  double StopWatchStart() {
      if(stopwatchcounting) stopwatch += record_time();
      else record_time();
      stopwatchcounting = 1;
      return stopwatch;
  }
};

// Returns medium resolution time  (~1ms)
// Since last call in floating point number
// Uses: ~.6 usec in overhead per call...
//
inline double record_time()
{
  static bool nfirst = false;
#ifdef linux
  static timeval ts_old;
  static timeval ts_new;
#else
  static timespec ts_old;
  static timespec ts_new;
#endif
  static double t;

  if(nfirst)
  {
#ifdef linux
    gettimeofday(&ts_new, NULL);
#else
    clock_gettime(CLOCK_REALTIME, &ts_new);
#endif
    t = ts_new.tv_sec - ts_old.tv_sec;

#ifdef linux
    t += ((double)(ts_new.tv_usec - ts_old.tv_usec))/1000000.0;
#else
    t += ((double)(ts_new.tv_nsec - ts_old.tv_nsec))/1000000000.0;
#endif
    ts_old = ts_new;
    return t;
  }

  nfirst = true;
#ifdef linux
  gettimeofday(&ts_old,NULL);
#else
  clock_gettime(CLOCK_REALTIME, &ts_old);
#endif
  return 0;
}

inline double record_abs_time(int set)
{
  static bool nfirst = false;
#ifdef linux
  static timeval ts_old;
  static timeval ts_new;
#else
  static timespec ts_old;
  static timespec ts_new;
#endif
  static double t;

  if(nfirst)
  {
#ifdef linux
    gettimeofday(&ts_new, NULL);
#else
    clock_gettime(CLOCK_REALTIME, &ts_new);
#endif
    t = ts_new.tv_sec - ts_old.tv_sec;

#ifdef linux
    t += ((double)(ts_new.tv_usec - ts_old.tv_usec))/1000000.0;
#else
    t += ((double)(ts_new.tv_nsec - ts_old.tv_nsec))/1000000000.0;
#endif
    //    ts_old = ts_new;
    return t;
  }

  if(set || nfirst) {
    nfirst = true;
#ifdef linux
    gettimeofday(&ts_old,NULL);
#else
    clock_gettime(CLOCK_REALTIME, &ts_old);
#endif
  }
  return 0;
}

// Inline wrappers for tonkos fast clocks:
//    RTS_CPUCYCLE type is a 64 or 32 bit int depending on platforms clock
//    
//    call getCpuCycle_init() once
//    call timeCpuCycle to time the clock
//    call setCpuCycleTime to set the cycle period yourself (in usecs)
//    call cpu2usec() to get the real time for some difference
static float cycle_mult = 0.0;

inline void getCpuCycle_init()
{
#ifdef vxworks
  mvmeFastTickerInit();
#endif
}

inline void setCpuCycleTime(float m)
{
  cycle_mult = m;
}

inline float getCpuCycleTime()
{
  return cycle_mult;
}

// Returns high resolution clock tick by platform
inline RTS_CPUCYCLE getCpuCycle64()
{
#if defined(i686) || defined(i386)
  return (RTS_CPUCYCLE)getfast_l();
#elif defined(vxworks)
  return (RTS_CPUCYCLE)mvmeFastTickerGet();
#else
    
  // If we don't have fast timer, fake it.
#ifdef linux
  timeval ts;
#else
  timespec ts;
#endif
  RTS_CPUCYCLE x;
#ifdef linux
  gettimeofday(&ts,NULL);
#else
  clock_gettime(CLOCK_REALTIME, &ts);
#endif

  x = ts.tv_sec;
  x *= 1000000000;
#ifdef linux
  x += ts.tv_usec*1000;
#else
  x += ts.tv_nsec;
#endif
  return x;
#endif
}

inline unsigned int getCpuCycle()
{
#if defined(i686) || defined(i386)
  return getfast();
#elif defined(vxworks)
  return mvmeFastTickerGet();
#else
  RTS_CPUCYCLE x = getCpuCycle64();
  return (unsigned int)(x & 0xffffffff);
#endif
}

inline void timeCpuCycle()
{
  RTS_CPUCYCLE t1, t2;

  int i=0;
  do {
    t1 = getCpuCycle();
#ifdef vxworks          // 1 second either way....
    taskDelay(100);
#else
    //timespec ts;
    //ts.tv_sec = 0;
    //ts.tv_nsec = 100000000;
    usleep((unsigned long)1000000);
    //nanosleep(&ts,NULL);
#endif
    t2 = getCpuCycle();

    cycle_mult = 1e6 / (float)(t2 - t1);    // cycle_mult usec/cycle
    i++;
  } while((t1 > t2) || (i>3));       // keep trying untill cycle mult > 0, but not too long
}

inline float cpu2usec(RTS_CPUCYCLE x)
{
  // printf("cycle_mult=%f %d %d \n",cycle_mult, (int)(x>>32),(int)x);
  return ((float)x) * cycle_mult;
}


// record time with high precision clock
#if defined linux && defined i686

inline double hrecord_time()
{
  static int first=1;

  static unsigned long long lt=0;
  unsigned long long t;
  unsigned long long et;
  static double clockpersec;
  
  if(first) {
    first = 0;
    lt = getfast_l();
    sleep(1);
    t = getfast_l();
    
    if(t > lt) 
      clockpersec = t-lt;
    else
      clockpersec = t + (0xffffffffffffffffLL - lt);


    lt = getfast_l();
    return 0.0;
  }

  t = getfast_l();
  
  if(t > lt) 
    et = t-lt;
  else
    et = t + (0xffffffffffffffffLL - lt);

  lt = t;

  return (((double)et) / clockpersec);
}
#endif

#endif
