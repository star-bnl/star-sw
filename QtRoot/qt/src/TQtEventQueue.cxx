// Author: Valeri Fine   25/03/2004

#include "TQtEventQueue.h"
#include "TQtLock.h"
#include <QApplication>
#include <cassert>


/****************************************************************************
** $Id: TQtEventQueue.cxx,v 1.6 2013/08/30 15:59:51 perev Exp $
**
** $$Copyright$
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

/////////////////////////////////////////////////////////////////////////////////
//
//  TQtEventQueue is a queue container of the pointers of Event_t structures 
//  created by TQtClientFilter class
//  If auto-deleting is turned on, all the items in a collection are deleted when 
//  the collection itself is deleted.
//  (for the full list of the members see: 
//  http://doc.trolltech.com/3.3/qptrlist.html)
//
/////////////////////////////////////////////////////////////////////////////////

//______________________________________________________________________________
TQtEventQueue::TQtEventQueue(): QQueue<const Event_t *> ()
{
   // Create the ROOT event queue
}

//______________________________________________________________________________
TQtEventQueue::~TQtEventQueue()
{
    // Remove all remaining events if any
    qDeleteAll(*this); 
}

//______________________________________________________________________________
int TQtEventQueue::RemoveItems(const Event_t *ev)
{ 
   // Removes all items matching ev->fWindow 
   // The removed item is deleted if auto-deletion (by default) is enabled
   // with class ctor
   
   // This method is used to debug the application only (by far)
   int counter = 0;
   assert(0);
   if (ev) { }
   return counter;
}

