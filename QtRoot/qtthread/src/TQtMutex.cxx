// @(#)root/thread:$Name:  $:$Id: TQtMutex.cxx,v 1.4 2013/08/30 16:00:28 perev Exp $
// $Id: TQtMutex.cxx,v 1.4 2013/08/30 16:00:28 perev Exp $
// Author: Valery Fine  08/25/2005
/****************************************************************************
** Copyright (C) 2005 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// TQtMutex                                                                //
//                                                                         //
// This class provides a Qt-based implementation of the TMutexImp interface//
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

#include "TThread.h"
#include "TQtMutex.h"
#include <QMutex>

ClassImp(TQtMutex)

//______________________________________________________________________________
TQtMutex::TQtMutex(Bool_t recursive)
{
   // Create a Qt mutex.
   fQMutex = recursive  ? new QMutex(QMutex::Recursive)
                        : new QMutex(QMutex::NonRecursive);
}

//______________________________________________________________________________
TQtMutex::~TQtMutex()
{
   // TMutex dtor.

  QMutex *m = fQMutex; fQMutex=0; delete m;
}

//______________________________________________________________________________
Int_t TQtMutex::Lock()
{
   // Lock the mutex.
   if (fQMutex) fQMutex->lock();
   return 0;
}

//______________________________________________________________________________
Int_t TQtMutex::TryLock()
{
   // Try locking the mutex. Returns 0 if mutex can be locked.

   Int_t locked = 1;
   if (fQMutex && fQMutex->tryLock()) locked = 0;
   return locked;
}

//______________________________________________________________________________
Int_t TQtMutex::UnLock(void)
{
   // Unlock the mutex.

   if (fQMutex) fQMutex->unlock();
   return 0;
}
