// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: QExObjectListItem.h,v 1.1 2006/12/06 15:18:48 fine Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtObjectListItem
#define ROOT_TQtObjectListItem

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtObjectListItem                                                    //
//                                                                      //
// The version of QCheckListItem with the pointer to ROOT TObject       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TSystemDirectory.h"
#include "TClass.h"

#include <qstring.h>
#include <qlistview.h>

#if (QT_VERSION > 0x0301000)
#  define _QCHECKLISTDEFAULT_ _QCHECKLISTDEFAULT_
#else
#  define _QCHECKLISTDEFAULT_ Controller
#endif


class TQtObjectListItem : public QObject, public QCheckListItem  {
protected:
  TObject           *fObject;
  Bool_t            fExpanded;  

public:
   TQtObjectListItem(TObject *obj, QCheckListItem *parent, const QString &text,
      Type itemType= _QCHECKLISTDEFAULT_ )
      : QCheckListItem(parent,text,itemType) ,fObject(obj),fExpanded(kFALSE){}

   TQtObjectListItem( TObject *obj, QCheckListItem *parent, QListViewItem *after,
         const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
      :QCheckListItem(parent,after,text,itemType),fObject(obj),fExpanded(kFALSE){}

   TQtObjectListItem( TObject *obj, QListViewItem *parent, QListViewItem *after,
            const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
      :QCheckListItem(parent,after,text,itemType),fObject(obj),fExpanded(kFALSE){}

   TQtObjectListItem( TObject *obj, QListView *parent, const QString &text,
               Type itemType= _QCHECKLISTDEFAULT_ )
      :QCheckListItem(parent,text,itemType),fObject(obj),fExpanded(kFALSE){}

   TQtObjectListItem( TObject *obj, QListView *parent, QListViewItem *after,
                  const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
      :QCheckListItem(parent, after,text, itemType),fObject(obj),fExpanded(kFALSE){}

   TQtObjectListItem( TObject *obj, QListViewItem *parent, const QString &text,
                     const QPixmap &pix )
       :QCheckListItem(parent,text,pix ),fObject(obj),fExpanded(kFALSE){}

   TQtObjectListItem( TObject *obj, QListView *parent, const QString &text,
                        const QPixmap &pix )
        :QCheckListItem(parent,text,pix),fObject(obj),fExpanded(kFALSE){}
  
     //______________________________________________________________________________
     void SetObject(TObject *obj) { fObject = obj; }
     //______________________________________________________________________________
     void Browse(TBrowser *b)     { if (fObject && b) fObject->Browse(b); }
     //______________________________________________________________________________
     void SetExpanded(Bool_t flag){ fExpanded = flag;}
     //______________________________________________________________________________
     Bool_t IsExpanded() const    { return fExpanded;}
     //______________________________________________________________________________
     TObject *Object() const      { return fObject;  }
     //______________________________________________________________________________
     TQtObjectListItem *Parent() const { return (TQtObjectListItem *)QListViewItem::parent(); }
     //______________________________________________________________________________
     TQtObjectListItem *RecursiveDelete (TObject *obj)
     {
        // Recursively delete all items containing the object "obj"
        QListViewItem *next = firstChild();
        while (next) {
           QListViewItem *nextBrother = next->nextSibling();
           delete ((TQtObjectListItem *)next)->RecursiveDelete(obj);
           next = nextBrother;
        };
        return !obj || (fObject == obj) ? this : 0 ;
     }
     //______________________________________________________________________________
     bool DoesChildrenContain(TObject *obj) 
     {
        QListViewItem *child = firstChild();
        if (obj->IsA() == TSystemFile::Class() || obj->IsA() == TSystemDirectory::Class()) {
           while (child &&  (QString(((TQtObjectListItem *)child)->Object()->GetName() ) != obj->GetName()) ) {
              child = child->nextSibling();
           }
        } else {
           while (child &&  (((TQtObjectListItem *)child)->Object() != obj) ) {
              child = child->nextSibling();
           }
        }
        return bool(child);
     }
     //______________________________________________________________________________
     const TQtObjectListItem *Find(TObject *obj) const {
        // look up the item to find out one with TObject pointer
        // recursively
        const TQtObjectListItem *thatFound = 0;
        if ( obj == fObject )
           thatFound =  this;
        else {
           QListViewItem *next = firstChild();
           while ( next && ! (thatFound =((TQtObjectListItem *)next)->Find(obj)) ) 
              next = next->nextSibling();
        }
        return thatFound;
     }
     //______________________________________________________________________________
     const TQtObjectListItem *FindByName(const char *objName) const {
        // look up the item to find out one with TObject name
        // recursively
        const TQtObjectListItem *thatFound = 0;
        if ( QString(objName)== fObject->GetName() ) 
           thatFound =  this;
        else {
           QListViewItem *next = firstChild();
           while ( next && ! (thatFound =((TQtObjectListItem *)next)->FindByName(objName)) ) 
              next = next->nextSibling();
        }
        return thatFound;
     }
};
#endif
