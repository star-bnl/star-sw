// @(#)root/thread:$Name:  $:$Id: TQtCondition.cxx,v 1.4 2013/08/30 16:00:28 perev Exp $
// Author: Bertrand Bellenot  20/10/2004

/*************************************************************************
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtCondition                                                         //
//                                                                      //
// This class provides an interface to the win32 condition variable     //
// routines.                                                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtCondition.h"
#include "TQtMutex.h"

#include <QWaitCondition> 
#include <QMutex>

ClassImp(TQtCondition)

//______________________________________________________________________________
TQtCondition::TQtCondition(TMutexImp *m):fMutex(((TQtMutex *) m)->Mutex())
{
   // Create QWaitCondition object. Ctor must be given a pointer to an
   // existing mutex. The condition variable is then linked to the mutex,
   // so that there is an implicit unlock and lock around Wait() and
   // TimedWait().

   fCond  = new QWaitCondition();
}

//______________________________________________________________________________
TQtCondition::~TQtCondition()
{
   // TCondition dtor.
   QWaitCondition *w = fCond; fCond = 0;  delete w;
}

//______________________________________________________________________________
Int_t TQtCondition::Wait()
{
   // Wait for the condition variable to be signalled. The mutex is
   // implicitely released before waiting and locked again after waking up.
   // If Wait() is called by multiple threads, a signal may wake up more
   // than one thread. See POSIX threads documentation for details.

   if (fCond) fCond->wait(&fMutex);
   return 0;
}

//______________________________________________________________________________
Int_t TQtCondition::TimedWait(ULong_t secs, ULong_t nanoSecs)
{
   // TimedWait() is given an absolute time to wait until. To wait for a
   // relative time from now, use TThread::GetTime(). See POSIX threads
   // documentation for why absolute times are better than relative.
   // Returns 0 if successfully signalled, 1 if time expired.

   Int_t timeOut = 1;
   if (fCond) {
      ULong_t dwMillisecondsNow = ((secs * 1000) + (nanoSecs / 1000000));
      if ( fCond->wait(&fMutex,dwMillisecondsNow) ) timeOut = 0;
   }
   return timeOut;
}

//______________________________________________________________________________
Int_t TQtCondition::Signal()
{
   // If one or more threads have called Wait(), Signal() wakes up at least
   // one of them, possibly more. See POSIX threads documentation for details.

   if (fCond) fCond->wakeOne();

   return 0;
}


//______________________________________________________________________________
Int_t TQtCondition::Broadcast()
{
   // Broadcast is like signal but wakes all threads which have called Wait().

   if (fCond) fCond->wakeAll();

   return 0;
}
