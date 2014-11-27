// @(#)root/thread:$Name:  $:$Id: TQtThreadImp.cxx,v 1.5 2013/08/30 16:00:29 perev Exp $
// $Id: TQtThreadImp.cxx,v 1.5 2013/08/30 16:00:29 perev Exp $
// Author: Valery Fine  08/25/2005
/****************************************************************************
** Copyright (C) 2005 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtThreadImp                                                         //
//                                                                      //
// This class provides an interface to the Qt QThread class             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

// ------------------------------
//
// The workaround to provide the Qt thread for the previous version of ROOT
// unless the class will be introduced as a legal friend officially by Fons :)
//
#define TWin32Thread TWin32Thread; friend class TQtThreadImp; friend class TRootThread;
#include "TQtThreadImp.h"
#undef TWin32Thread
// ------------------------------
#include <QThread>
//
// TRootThread - is a hack to gain an access to the private "fHanle" data-member
//
class TRootThread : public QThread {
  private:
    TThread *fROOTThread;
  public:
     TRootThread(TThread *thread) : fROOTThread(thread) {}
     virtual ~TRootThread() {
        fROOTThread->fHandle = 0L;
     }
     virtual void run() { 
           fROOTThread->fId = (Long_t)QThread::currentThread();
           TThread::Function(fROOTThread); 
     }
};
//
// TCancelThread 
//
class TCancelThread : public QThread {
   private:
     TCancelThread() {};
   public:
     ~TCancelThread() {};
   void SetCancel(Bool_t on=kTRUE) {
#if (QT_VERSION >= 0x040000)
      setTerminationEnabled(on);
#endif      
   }               
};

ClassImp(TQtThreadImp)

//______________________________________________________________________________
Int_t TQtThreadImp::Run(TThread *th)
{
   // Qt threads -- spawn new thread (like pthread_create).
   // Qt has a thread QThread pointer in addition to the thread ID.

   QThread *thread = new  TRootThread(th);
   th->fHandle = (Long_t)thread;
   QThread::Priority pri = QThread::InheritPriority;
   switch (th->GetPriority()) {
     case  TThread::kLowPriority:   pri = QThread::LowPriority;     break;
     case  TThread::kNormalPriority:pri = QThread::InheritPriority; break;
     case  TThread::kHighPriority:  pri = QThread::HighPriority;    break;
   };
   thread->start(pri);
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::Join(TThread *th, void ** /*ret*/)
{
   // Join  suspends  the  execution  of the calling thread until the
   // thread identified by th terminates, Exit or by being cancelled.
   
   QThread *qt = ((QThread*)th->fHandle);
   // QThread:;wait() provides similar functionality 
   // to the POSIX pthread_join() function.
   return qt ? (qt->wait() ? 0 : -1) : 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::Exit(void * /*ret*/)
{
   // Terminates the execution of the calling thread.

#if (QT_VERSION < 0x040000)
   QThread::exit();
#else
   QThread *qt = QThread::currentThread();
   if (qt)    qt->terminate();
#endif   
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::Kill(TThread *th)
{
   // Cancellation is the mechanism by which a thread can terminate the
   // execution of another thread. (why is it called "Kill" ??? ) 

   QThread *qt = ((QThread*)th->fHandle);
   if (qt) { qt->terminate();  qt->wait(); delete qt;  th->fHandle = 0L;}
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::CleanUpPush(void **main, void *free,void *arg)
{
   if (!free) fprintf(stderr, "CleanUpPush ***ERROR*** Routine=0\n");
   new TQtThreadImpCleanUp(main,free,arg);
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::CleanUpPop(void **main,Int_t exe)
{
   if (!*main) return 1;
   TQtThreadImpCleanUp *l = (TQtThreadImpCleanUp*)(*main);
   if (!l->fRoutine) fprintf(stderr,"CleanUpPop ***ERROR*** Routine=0\n");
   if (exe && l->fRoutine) ((void (*)(void*))(l->fRoutine))(l->fArgument);
   *main = l->fNext;  delete l;
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::CleanUp(void **main)
{
   fprintf(stderr," CleanUp %lx\n",(ULong_t)*main);
   while(!CleanUpPop(main,1)) { }
   return 0;
}

//______________________________________________________________________________
Long_t TQtThreadImp::SelfId()
{
   // Return the thread identifier for the calling thread.

   return (Long_t)QThread::currentThread();
}

//______________________________________________________________________________
Int_t TQtThreadImp::SetCancelOff()
{
   // Turn off the cancellation state of the calling thread.
#if (QT_VERSION < 0x040000)
   if (gDebug)
      Warning("SetCancelOff", "Not implemented on Qt");
#else
   QThread *qt = QThread::currentThread();
   if (qt) ((TCancelThread *)qt)->SetCancel(kFALSE);
#endif      
   
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::SetCancelOn()
{
   // Turn on the cancellation state of the calling thread.
#if (QT_VERSION < 0x040000)
   if (gDebug)
      Warning("SetCancelOn", "Not implemented on Qt ");
#else
   QThread *qt = QThread::currentThread();
   if (qt) ((TCancelThread *)qt)->SetCancel();
#endif      
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::SetCancelAsynchronous()
{
   // Set the cancellation response type of the calling thread to
   // asynchronous, i.e. cancel as soon as the cancellation request
   // is received.

   if (gDebug)
      Warning("SetCancelAsynchronous", "Not implemented on Qt");
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::SetCancelDeferred()
{
   // Set the cancellation response type of the calling thread to
   // deferred, i.e. cancel only at next cancellation point.
   
   if (gDebug)
      Warning("SetCancelDeferred", "Not implemented on Qt");
   return 0;
}

//______________________________________________________________________________
Int_t TQtThreadImp::CancelPoint()
{
   // Introduce an explicit cancellation point.
   if (gDebug)
      Warning("CancelPoint", "Not implemented on Qt");
   return 0;
}

//   Clean Up section. PTHREAD implementations of cleanup after cancel are
//   too different and often too bad. Temporary I invent my own bicycle.
//                                                              V.Perev.

//______________________________________________________________________________
TQtThreadImpCleanUp::TQtThreadImpCleanUp(void **main, void *routine, void *arg)
{
   fNext = (TQtThreadImpCleanUp*)*main;
   fRoutine = routine; fArgument = arg;
   *main  = this;
}
