// @(#)root/thread:$Name:  $:$Id: TQtMutex.h,v 1.4 2013/08/30 16:00:28 perev Exp $
// $Id: TQtMutex.h,v 1.4 2013/08/30 16:00:28 perev Exp $
// Author: Valery Fine  08/25/2005
/****************************************************************************
** Copyright (C) 2005 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifndef ROOT_TQtMutex
#define ROOT_TQtMutex


/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// TQtMutex                                                                //
//                                                                         //
// This class provides a Qt-based implementation of the TMutexImp interface//
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TMutexImp
#include "TMutexImp.h"
#endif

class QMutex;

class TQtMutex : public TMutexImp {

friend class TQtCondition;

private:
   QMutex *fQMutex;

public:
   TQtMutex(Bool_t recursive=kFALSE);
   virtual ~TQtMutex();

   virtual Int_t  Lock();
   virtual Int_t  UnLock();
   virtual Int_t  TryLock();
// 
   const QMutex &Mutex() const;
   QMutex &Mutex();
   ClassDef(TQtMutex,0)  // Qt-based implementation of the TMutexImp interface
};

inline  const QMutex &TQtMutex::Mutex() const { return *fQMutex; }
inline        QMutex &TQtMutex::Mutex()       { return *fQMutex; }

#endif
