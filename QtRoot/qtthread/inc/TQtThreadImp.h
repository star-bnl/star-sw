// @(#)root/thread:$Name:  $:$Id: TQtThreadImp.h,v 1.4 2013/08/30 16:00:28 perev Exp $
// $Id: TQtThreadImp.h,v 1.4 2013/08/30 16:00:28 perev Exp $
// Author: Valery Fine  08/25/2005
/****************************************************************************
** Copyright (C) 2005 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifndef ROOT_TQtThreadImp
#define ROOT_TQtThreadImp


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtThreadImp                                                         //
//                                                                      //
// This class provides an interface to the QThread thread class         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TThreadImp
#include "TThreadImp.h"
#endif

class TQtThreadImpCleanUp;

class TQtThreadImp : public TThreadImp {

private:
   TQtThreadImpCleanUp *fCleanUp;

public:
    TQtThreadImp()  { }
   ~TQtThreadImp() { }

   virtual Int_t  Join(TThread *th, void **ret);
   virtual Long_t SelfId();
   virtual Int_t  Run(TThread *th);

   virtual Int_t  Kill(TThread *th);

   virtual Int_t  SetCancelOff();
   virtual Int_t  SetCancelOn();
   virtual Int_t  SetCancelAsynchronous();
   virtual Int_t  SetCancelDeferred();
   virtual Int_t  CancelPoint();

   virtual Int_t  CleanUpPush(void **main, void *free,void *arg);
   virtual Int_t  CleanUpPop(void **main, Int_t exe);
   virtual Int_t  CleanUp(void **main);

   virtual Int_t  Exit(void *ret);

   ClassDef(TQtThreadImp,0)  // TQtThreadImp class
};


class TQtThreadImpCleanUp {

friend class TQtThreadImp;

private:
   void                *fRoutine;
   void                *fArgument;
   TQtThreadImpCleanUp *fNext;

public:
   TQtThreadImpCleanUp(void **main,void *routine,void *arg);
   ~TQtThreadImpCleanUp() { }
};

#endif
