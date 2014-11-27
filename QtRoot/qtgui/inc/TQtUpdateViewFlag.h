#ifndef ROOT_TQTUPDATEVIEWFLAG
#define ROOT_TQTUPDATEVIEWFLAG
// Author: Valeri Fine   30/04/2003
/****************************************************************************
** $Id: TQtUpdateViewFlag.h,v 1.4 2013/08/30 16:00:22 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
#include <QtGlobal>
#include <q3scrollview.h> 

/////////////////////////////////////////////////////////////
//                                                         //
//  Small counter class to avoid redundant lock/ unlock    //
//  Scrollview to update                                   //
//                                                         //
/////////////////////////////////////////////////////////////
class TQtUpdateViewFlag {
private:
   unsigned int  fCounter;
   TQtUpdateViewFlag(const TQtUpdateViewFlag &){;}
public:
   TQtUpdateViewFlag(): fCounter (0){};
   void FreezeToUpdate(Q3ScrollView *view){
      if (!fCounter) {
         view->setUpdatesEnabled( FALSE );
         view->viewport()->setUpdatesEnabled( FALSE );
      }
      fCounter++;
   };
   void UnFreezeToUpdate(Q3ScrollView *view){ 
      if (fCounter) fCounter--; 
      if (!fCounter) {
         view->viewport()->setUpdatesEnabled( true );
         view->setUpdatesEnabled( true );
         view->repaintContents();
      }
   }
};

#endif


