// @(#)root/base:$Name:  $:$Id: Stopwatch.h,v 1.2 2016/06/21 03:39:45 smirnovd Exp $
// Author: Fons Rademakers   11/10/95

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef _Stopwatch
#define _Stopwatch


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TStopwatch                                                           //
//                                                                      //
// Stopwatch class. This class returns the real and cpu time between    //
// the start and stop events.                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


class Stopwatch {

private:
   enum EState { kUndefined, kStopped, kRunning };

   double     fStartRealTime;   //wall clock start time
   double     fStopRealTime;    //wall clock stop time
   double     fStartCpuTime;    //cpu start time
   double     fStopCpuTime;     //cpu stop time
   double     fTotalCpuTime;    //total cpu time
   double     fTotalRealTime;   //total real time
   EState       fState;           //stopwatch state
   int        fCounter;         //number of times the stopwatch was started

   inline static double GetRealTime();
   inline static double GetCPUTime();

public:
   inline Stopwatch();
   inline void        Start(int reset = 1);
   inline void        Stop();
   inline void        Continue();
   inline int         Counter() const { return fCounter; }
   inline double      RealTime();
   inline void        Reset() { ResetCpuTime(); ResetRealTime(); }
   inline void        ResetCpuTime(double time = 0) { Stop();  fTotalCpuTime = time; }
   inline void        ResetRealTime(double time = 0) { Stop(); fTotalRealTime = time; }
   inline double      CpuTime();
};


//______________________________________________________________________________

#ifndef _WIN32
#define R__UNIX 
#endif

#if defined(R__UNIX)
#  include <sys/time.h>
#  include <sys/times.h>
#  include <unistd.h>
static double gTicks = 0;
#elif defined(R__VMS)
#  include <time.h>
#  include <unistd.h>
static double gTicks = 1000;
#elif defined(_WIN32)
#  include <sys/types.h> 
#  include <sys/timeb.h>
#  include <windows.h>
#  undef min // unfortunately, <windows.h> defines min/max as macros
#  undef max
//#  include "TError.h"
const double gTicks = 1.0e-7;
static __int64 gTicksQPC = -1; // < 0 means "not yet initialized" 
//#  include "Windows4Root.h"
#endif


inline Stopwatch::Stopwatch():
 fStartRealTime(0), fStopRealTime(0), fStartCpuTime(0), fStopCpuTime(0),
 fTotalCpuTime(0), fTotalRealTime(0), fState(kUndefined), fCounter(0)
{
   // Create a stopwatch and start it.

#ifdef R__UNIX
   if (!gTicks)
      gTicks = (double)sysconf(_SC_CLK_TCK);
#endif
#ifdef _WIN32
    if( gTicksQPC < 0 ) {
        LARGE_INTEGER freq;
        QueryPerformanceFrequency( &freq );
	gTicksQPC = (double)freq.QuadPart;
    }
#endif

   Start();
}

//______________________________________________________________________________
inline void Stopwatch::Start(int reset)
{
   // Start the stopwatch. If reset is kTRUE reset the stopwatch before
   // starting it (including the stopwatch counter).
   // Use kFALSE to continue timing after a Stop() without
   // resetting the stopwatch.

   if (reset) {
      fState         = kUndefined;
      fTotalCpuTime  = 0;
      fTotalRealTime = 0;
      fCounter       = 0;
   }
   if (fState != kRunning) {
      fStartRealTime = GetRealTime();
      fStartCpuTime  = GetCPUTime();
   }
   fState = kRunning;
   fCounter++;
}

//______________________________________________________________________________
inline void Stopwatch::Stop()
{
   // Stop the stopwatch.

   fStopRealTime = GetRealTime();
   fStopCpuTime  = GetCPUTime();

   if (fState == kRunning) {
      fTotalCpuTime  += fStopCpuTime  - fStartCpuTime;
      fTotalRealTime += fStopRealTime - fStartRealTime;
   }
   fState = kStopped;
}

//______________________________________________________________________________
inline void Stopwatch::Continue()
{
   // Resume a stopped stopwatch. The stopwatch continues counting from the last
   // Start() onwards (this is like the laptimer function).

  if (fState == kUndefined){
    //cout<< "stopwatch not started"<<endl;
    return;
  }
   if (fState == kStopped) {
      fTotalCpuTime  -= fStopCpuTime  - fStartCpuTime;
      fTotalRealTime -= fStopRealTime - fStartRealTime;
   }

   fState = kRunning;
}

//______________________________________________________________________________
inline double Stopwatch::RealTime()
{
   // Return the realtime passed between the start and stop events. If the
   // stopwatch was still running stop it first.

  if (fState == kUndefined){
    //cout<<"stopwatch not started"<<endl;
    return 0;
  }
   if (fState == kRunning)
      Stop();

   return fTotalRealTime;
}

//______________________________________________________________________________
inline double Stopwatch::CpuTime()
{
   // Return the cputime passed between the start and stop events. If the
   // stopwatch was still running stop it first.

   if (fState == kUndefined){
     //cout<<"stopwatch not started"<<endl;
     return 0;
   }
   if (fState == kRunning)
      Stop();

   return fTotalCpuTime;
}

//______________________________________________________________________________
inline double Stopwatch::GetRealTime()
{
#if defined(R__UNIX)
  struct timeval tp;
  gettimeofday(&tp, 0);
  return tp.tv_sec + (tp.tv_usec)*1.e-6;
#elif defined(_WIN32)
  LARGE_INTEGER counter;
  QueryPerformanceCounter( &counter );
  return (double)counter.QuadPart / gTicksQPC;
#else    
  return 0;
#endif
}

//______________________________________________________________________________
inline double Stopwatch::GetCPUTime()
{
   // Private static method returning system CPU time.

#if defined(R__UNIX)
   struct tms cpt;
   times(&cpt);
   return (double)(cpt.tms_utime+cpt.tms_stime) / gTicks;
#elif defined(R__VMS)
   return (double)clock() / gTicks;
#elif defined(_WIN32)
   OSVERSIONINFO OsVersionInfo;

   //         Value                      Platform
   //----------------------------------------------------
   //  VER_PLATFORM_WIN32s          Win32s on Windows 3.1
   //  VER_PLATFORM_WIN32_WINDOWS   Win32 on Windows 95
   //  VER_PLATFORM_WIN32_NT        Windows NT
   //
   OsVersionInfo.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
   GetVersionEx(&OsVersionInfo);
   if (OsVersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT) {
      DWORD       ret;
      FILETIME    ftCreate,       // when the process was created
                  ftExit;         // when the process exited

      union {
         FILETIME ftFileTime;
         __int64  ftInt64;
      } ftKernel; // time the process has spent in kernel mode

      union {
         FILETIME ftFileTime;
         __int64  ftInt64;
      } ftUser;   // time the process has spent in user mode

      HANDLE hProcess = GetCurrentProcess();
      ret = GetProcessTimes (hProcess, &ftCreate, &ftExit,
                                       &ftKernel.ftFileTime,
                                       &ftUser.ftFileTime);
      if (ret != TRUE) {
	ret = GetLastError ();
	//cout<<" Error on GetProcessTimes 0x%lx"<<endl;
	return 0;
      }

      // Process times are returned in a 64-bit structure, as the number of
      // 100 nanosecond ticks since 1 January 1601.  User mode and kernel mode
      // times for this process are in separate 64-bit structures.
      // To convert to floating point seconds, we will:
      //
      // Convert sum of high 32-bit quantities to 64-bit int

      return (double) (ftKernel.ftInt64 + ftUser.ftInt64) * gTicks;
   } else
      return GetRealTime();
#endif
}

#endif
