#ifndef ROOT_TWaitCondition
#define ROOT_TWaitCondition

// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TWaitCondition.h,v 1.4 2013/08/30 15:59:50 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/
#include <limits.h>
#include "TQtRConfig.h"
#ifdef R__QTGUITHREAD
#include "TWin32Semaphore.h"

class TWaitCondition : public TWin32Semaphore 
{
   public:
     TWaitCondition() : TWin32Semaphore() {}
     ~TWaitCondition() {}
     bool wait (unsigned long time= ULONG_MAX) { Wait(); return true;}
     void wakeOne () { Release(); }
};
#else
// An dummy version for the "non-thread" implementations
class TWaitCondition
{
   public:
     TWaitCondition()  {}
     ~TWaitCondition() {}
	  bool wait (unsigned long time=ULONG_MAX ) { if (time) {/* Wait() */}  return true;}
     void wakeOne () { /* Release();*/  }
};

#endif
#endif
