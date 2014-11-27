// @(#)root/thread:$Name:  $:$Id: TQtThreadFactory.cxx,v 1.4 2013/08/30 16:00:29 perev Exp $
// $Id: TQtThreadFactory.cxx,v 1.4 2013/08/30 16:00:29 perev Exp $
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
// TQtThreadFactory                                                     //
//                                                                      //
// This is a factory for Win32 thread components.                       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtThreadFactory.h"
#include "TQtMutex.h"
#include "TQtCondition.h"
#include "TQtThreadImp.h"

// Force creation of TQtThreadFactory when shared library will be loaded
// (don't explicitely create a TQtThreadFactory).

static TQtThreadFactory gQtThreadFactoryCreator;

ClassImp(TQtThreadFactory)

//______________________________________________________________________________
TQtThreadFactory::TQtThreadFactory(const char *name, const char *title) :
                     TThreadFactory(name, title)
{
   // Create Q thread factory. Also sets global gThreadFactory to this.

   gThreadFactory = this;
}

//______________________________________________________________________________
TMutexImp *TQtThreadFactory::CreateMutexImp(Bool_t recursive)
{
   // Return a Win32 Mutex.

   return new TQtMutex(recursive);
}

//______________________________________________________________________________
TMutexImp *TQtThreadFactory::CreateMutexImp()
{
   // Return a Win32 Mutex.

   return new TQtMutex;
}

//______________________________________________________________________________
TThreadImp *TQtThreadFactory::CreateThreadImp()
{
   // Return a Q thread.

   return new TQtThreadImp();
}

//______________________________________________________________________________
TConditionImp *TQtThreadFactory::CreateConditionImp(TMutexImp *m)
{
   // Return a Q condition variable.

   return new TQtCondition(m);
}
