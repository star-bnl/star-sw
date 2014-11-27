// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtObjectListItem.h,v 1.5 2013/08/30 16:00:22 perev Exp $
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
#if QT_VERSION < 0x40000
#include <qlistview.h>
#else /* QT_VERSION */
#include <q3listview.h>
//Added by qt3to4:
#include <QPixmap>
#endif /* QT_VERSION */

#if (QT_VERSION > 0x0301000)
#  define _QCHECKLISTDEFAULT_ _QCHECKLISTDEFAULT_
#else
#  define _QCHECKLISTDEFAULT_ Controller
#endif

#if 1

class TQtObjectListItemInterface : public QObject {
protected:
  TObject             *fObject;
#if QT_VERSION < 0x40000
  const QListViewItem *fThisQtItem;
#else /* QT_VERSION */
  const Q3ListViewItem *fThisQtItem;
#endif /* QT_VERSION */
  Bool_t             fExpanded;  
  Bool_t             fIsCheckedType;  

protected:
#if QT_VERSION < 0x40000
   TQtObjectListItemInterface(QListViewItem *thisItem, TObject *obj, Bool_t checked=kFALSE, Bool_t expand=kFALSE)
#else /* QT_VERSION */
   TQtObjectListItemInterface(Q3ListViewItem *thisItem, TObject *obj, Bool_t checked=kFALSE, Bool_t expand=kFALSE)
#endif /* QT_VERSION */
      : fObject(obj),fThisQtItem(thisItem),fExpanded(expand),fIsCheckedType(checked) {}
public:
     //______________________________________________________________________________
     virtual ~TQtObjectListItemInterface() {fObject =0;}  
     //______________________________________________________________________________
     void SetObject(TObject *obj) { fObject = obj; }
     //______________________________________________________________________________
     void Browse(TBrowser *b)     { if (fObject && b) fObject->Browse(b); }
     //______________________________________________________________________________
     void SetExpanded(Bool_t flag){ fExpanded = flag;}
     //______________________________________________________________________________
     Bool_t IsExpanded() const    { return fExpanded;}
     //______________________________________________________________________________
     Bool_t IsCheckedType() const  { return fIsCheckedType;}
     //______________________________________________________________________________
     TObject *Object() const      { return fObject;  }
     //______________________________________________________________________________
#if QT_VERSION < 0x40000
     static TQtObjectListItemInterface *Cast(QListViewItem *thisItem);
#else /* QT_VERSION */
     static TQtObjectListItemInterface *Cast(Q3ListViewItem *thisItem);
#endif /* QT_VERSION */
     //______________________________________________________________________________
     TQtObjectListItemInterface *Parent() const { return Cast(fThisQtItem->parent()); }
     //______________________________________________________________________________
     TQtObjectListItemInterface *RecursiveDelete (TObject *obj)
     {
        // Recursively delete all items containing the object "obj"
#if QT_VERSION < 0x40000
        QListViewItem *next = fThisQtItem->firstChild();
#else /* QT_VERSION */
        Q3ListViewItem *next = fThisQtItem->firstChild();
#endif /* QT_VERSION */
        while (next) {
#if QT_VERSION < 0x40000
           QListViewItem *nextBrother = next->nextSibling();
#else /* QT_VERSION */
           Q3ListViewItem *nextBrother = next->nextSibling();
#endif /* QT_VERSION */
           delete Cast((next))->RecursiveDelete(obj);
           next = nextBrother;
        };
        return !obj || (fObject == obj) ? this : 0 ;
     }
     //______________________________________________________________________________
     bool DoesChildrenContain(TObject *obj) 
     {
#if QT_VERSION < 0x40000
        QListViewItem *child = fThisQtItem->firstChild();
#else /* QT_VERSION */
        Q3ListViewItem *child = fThisQtItem->firstChild();
#endif /* QT_VERSION */
        if (obj->IsA() == TSystemFile::Class() || obj->IsA() == TSystemDirectory::Class()) {
           while (child &&  (QString(Cast(child)->Object()->GetName() ) != obj->GetName()) ) {
              child = child->nextSibling();
           }
        } else {
           while (child &&  ((Cast(child)->Object() != obj) ) ) {
              child = child->nextSibling();
           }
        }
        return bool(child);
     }
     //______________________________________________________________________________
     const TQtObjectListItemInterface *Find(TObject *obj) const {
        // look up the item to find out one with TObject pointer
        // recursively
        const TQtObjectListItemInterface *thatFound = 0;
        if ( obj == fObject )
           thatFound =  this;
        else {
#if QT_VERSION < 0x40000
           QListViewItem *next = fThisQtItem->firstChild();
#else /* QT_VERSION */
           Q3ListViewItem *next = fThisQtItem->firstChild();
#endif /* QT_VERSION */
           while ( next && ! (thatFound =(Cast(next)->Find(obj))) )
              next = next->nextSibling();
        }
        return thatFound;
     }
     //______________________________________________________________________________
     const TQtObjectListItemInterface *FindByName(const char *objName) const {
        // look up the item to find out one with TObject name
        // recursively
        const TQtObjectListItemInterface *thatFound = 0;
        if ( QString(objName)== fObject->GetName() ) 
           thatFound =  this;
        else {
#if QT_VERSION < 0x40000
           QListViewItem *next = fThisQtItem->firstChild();
#else /* QT_VERSION */
           Q3ListViewItem *next = fThisQtItem->firstChild();
#endif /* QT_VERSION */
           while ( next && ! (thatFound =(Cast(next)->FindByName(objName))) ) 
              next = next->nextSibling();
        }
        return thatFound;
     }
};

//______________________________________________________________________________
//
//  TQtObjectListItem  - check ROOT item for root ListView 
//______________________________________________________________________________
#if QT_VERSION < 0x40000
class TQtObjectListItem : public TQtObjectListItemInterface, public QCheckListItem  {
#else /* QT_VERSION */
class TQtObjectListItem : public TQtObjectListItemInterface, public Q3CheckListItem  {
#endif /* QT_VERSION */

public:
#if QT_VERSION < 0x40000
   TQtObjectListItem(TObject *obj, QCheckListItem *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem(TObject *obj, Q3CheckListItem *parent, const QString &text,
#endif /* QT_VERSION */
      Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      : TQtObjectListItemInterface(this,obj,kTRUE),QCheckListItem(parent,text,itemType){}
#else /* QT_VERSION */
      : TQtObjectListItemInterface(this,obj,kTRUE),Q3CheckListItem(parent,text,itemType){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QCheckListItem *parent, QListViewItem *after,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3CheckListItem *parent, Q3ListViewItem *after,
#endif /* QT_VERSION */
         const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :TQtObjectListItemInterface(this,obj,kTRUE),QCheckListItem(parent,after,text,itemType){}
#else /* QT_VERSION */
      :TQtObjectListItemInterface(this,obj,kTRUE),Q3CheckListItem(parent,after,text,itemType){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListViewItem *parent, QListViewItem *after,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListViewItem *parent, Q3ListViewItem *after,
#endif /* QT_VERSION */
            const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :TQtObjectListItemInterface(this,obj,kTRUE),QCheckListItem(parent,after,text,itemType){}
#else /* QT_VERSION */
      :TQtObjectListItemInterface(this,obj,kTRUE),Q3CheckListItem(parent,after,text,itemType){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListView *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListView *parent, const QString &text,
#endif /* QT_VERSION */
               Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :TQtObjectListItemInterface(this,obj,kTRUE),QCheckListItem(parent,text,itemType){}
#else /* QT_VERSION */
      :TQtObjectListItemInterface(this,obj,kTRUE),Q3CheckListItem(parent,text,itemType){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListView *parent, QListViewItem *after,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListView *parent, Q3ListViewItem *after,
#endif /* QT_VERSION */
                  const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :TQtObjectListItemInterface(this,obj,kTRUE),QCheckListItem(parent, after,text, itemType){}
#else /* QT_VERSION */
      :TQtObjectListItemInterface(this,obj,kTRUE),Q3CheckListItem(parent, after,text, itemType){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListViewItem *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListViewItem *parent, const QString &text,
#endif /* QT_VERSION */
                     const QPixmap &pix )
#if QT_VERSION < 0x40000
       :TQtObjectListItemInterface(this,obj,kTRUE),QCheckListItem(parent,text,pix ){}
#else /* QT_VERSION */
       :TQtObjectListItemInterface(this,obj,kTRUE),Q3CheckListItem(parent,text,pix ){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListView *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListView *parent, const QString &text,
#endif /* QT_VERSION */
                        const QPixmap &pix )
#if QT_VERSION < 0x40000
        :TQtObjectListItemInterface(this,obj,kTRUE), QCheckListItem(parent,text,pix){}
#else /* QT_VERSION */
        :TQtObjectListItemInterface(this,obj,kTRUE), Q3CheckListItem(parent,text,pix){}
#endif /* QT_VERSION */
  
};

//______________________________________________________________________________
//
//  TQtBrowserItem  - check ROOT item for root ListView 
//______________________________________________________________________________
#if QT_VERSION < 0x40000
class TQtBrowserItem : public TQtObjectListItemInterface, public QListViewItem {
#else /* QT_VERSION */
class TQtBrowserItem : public TQtObjectListItemInterface, public Q3ListViewItem {
#endif /* QT_VERSION */

public:

#if QT_VERSION < 0x40000
  TQtBrowserItem( TObject *obj,QListView * parent, QString label1) 
     : TQtObjectListItemInterface(this,obj), QListViewItem(parent,label1)  {;}
  TQtBrowserItem( TObject *obj,QListView * parent, QString label1,QString label2)
     : TQtObjectListItemInterface(this,obj), QListViewItem(parent,label1)  {;}
  TQtBrowserItem( TObject *obj,QListViewItem * parent, QString label1)
     : TQtObjectListItemInterface(this,obj), QListViewItem(parent,label1)  {;}
  TQtBrowserItem( TObject *obj,QListViewItem * parent, QString label1,QString label2)
     : TQtObjectListItemInterface(this,obj), QListViewItem(parent,label1,label2) {;}
  TQtBrowserItem( TObject *obj,QListView * parent, QListViewItem * after )
     : TQtObjectListItemInterface(this,obj), QListViewItem(parent,after )  {;}
  TQtBrowserItem( TObject *obj,QListViewItem * parent, QListViewItem * after )
     : TQtObjectListItemInterface(this,obj), QListViewItem(parent,after )  {;}
#else /* QT_VERSION */
  TQtBrowserItem( TObject *obj,Q3ListView * parent, QString label1) 
     : TQtObjectListItemInterface(this,obj), Q3ListViewItem(parent,label1)  {;}
  TQtBrowserItem( TObject *obj,Q3ListView * parent, QString label1,QString label2)
     : TQtObjectListItemInterface(this,obj), Q3ListViewItem(parent,label1)  {;}
  TQtBrowserItem( TObject *obj,Q3ListViewItem * parent, QString label1)
     : TQtObjectListItemInterface(this,obj), Q3ListViewItem(parent,label1)  {;}
  TQtBrowserItem( TObject *obj,Q3ListViewItem * parent, QString label1,QString label2)
     : TQtObjectListItemInterface(this,obj), Q3ListViewItem(parent,label1,label2) {;}
  TQtBrowserItem( TObject *obj,Q3ListView * parent, Q3ListViewItem * after )
     : TQtObjectListItemInterface(this,obj), Q3ListViewItem(parent,after )  {;}
  TQtBrowserItem( TObject *obj,Q3ListViewItem * parent, Q3ListViewItem * after )
     : TQtObjectListItemInterface(this,obj), Q3ListViewItem(parent,after )  {;}
#endif /* QT_VERSION */
};


#else



#if QT_VERSION < 0x40000
class TQtObjectListItem : public QObject, public QCheckListItem  {
#else /* QT_VERSION */
class TQtObjectListItem : public QObject, public Q3CheckListItem  {
#endif /* QT_VERSION */
protected:
  TObject           *fObject;
  Bool_t            fExpanded;  

public:
#if QT_VERSION < 0x40000
   TQtObjectListItem(TObject *obj, QCheckListItem *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem(TObject *obj, Q3CheckListItem *parent, const QString &text,
#endif /* QT_VERSION */
      Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      : QCheckListItem(parent,text,itemType) ,fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
      : Q3CheckListItem(parent,text,itemType) ,fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QCheckListItem *parent, QListViewItem *after,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3CheckListItem *parent, Q3ListViewItem *after,
#endif /* QT_VERSION */
         const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :QCheckListItem(parent,after,text,itemType),fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
      :Q3CheckListItem(parent,after,text,itemType),fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListViewItem *parent, QListViewItem *after,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListViewItem *parent, Q3ListViewItem *after,
#endif /* QT_VERSION */
            const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :QCheckListItem(parent,after,text,itemType),fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
      :Q3CheckListItem(parent,after,text,itemType),fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListView *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListView *parent, const QString &text,
#endif /* QT_VERSION */
               Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :QCheckListItem(parent,text,itemType),fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
      :Q3CheckListItem(parent,text,itemType),fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListView *parent, QListViewItem *after,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListView *parent, Q3ListViewItem *after,
#endif /* QT_VERSION */
                  const QString &text, Type itemType= _QCHECKLISTDEFAULT_ )
#if QT_VERSION < 0x40000
      :QCheckListItem(parent, after,text, itemType),fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
      :Q3CheckListItem(parent, after,text, itemType),fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListViewItem *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListViewItem *parent, const QString &text,
#endif /* QT_VERSION */
                     const QPixmap &pix )
#if QT_VERSION < 0x40000
       :QCheckListItem(parent,text,pix ),fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
       :Q3CheckListItem(parent,text,pix ),fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   TQtObjectListItem( TObject *obj, QListView *parent, const QString &text,
#else /* QT_VERSION */
   TQtObjectListItem( TObject *obj, Q3ListView *parent, const QString &text,
#endif /* QT_VERSION */
                        const QPixmap &pix )
#if QT_VERSION < 0x40000
        :QCheckListItem(parent,text,pix),fObject(obj),fExpanded(kFALSE){}
#else /* QT_VERSION */
        :Q3CheckListItem(parent,text,pix),fObject(obj),fExpanded(kFALSE){}
#endif /* QT_VERSION */
  
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
#if QT_VERSION < 0x40000
     TQtObjectListItem *Parent() const { return (TQtObjectListItem *)QListViewItem::parent(); }
#else /* QT_VERSION */
     TQtObjectListItem *Parent() const { return (TQtObjectListItem *)Q3ListViewItem::parent(); }
#endif /* QT_VERSION */
     //______________________________________________________________________________
     TQtObjectListItem *RecursiveDelete (TObject *obj)
     {
        // Recursively delete all items containing the object "obj"
#if QT_VERSION < 0x40000
        QListViewItem *next = firstChild();
#else /* QT_VERSION */
        Q3ListViewItem *next = firstChild();
#endif /* QT_VERSION */
        while (next) {
#if QT_VERSION < 0x40000
           QListViewItem *nextBrother = next->nextSibling();
#else /* QT_VERSION */
           Q3ListViewItem *nextBrother = next->nextSibling();
#endif /* QT_VERSION */
           delete ((TQtObjectListItem *)next)->RecursiveDelete(obj);
           next = nextBrother;
        };
        return !obj || (fObject == obj) ? this : 0 ;
     }
     //______________________________________________________________________________
     bool DoesChildrenContain(TObject *obj) 
     {
#if QT_VERSION < 0x40000
        QListViewItem *child = firstChild();
#else /* QT_VERSION */
        Q3ListViewItem *child = firstChild();
#endif /* QT_VERSION */
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
#if QT_VERSION < 0x40000
           QListViewItem *next = firstChild();
#else /* QT_VERSION */
           Q3ListViewItem *next = firstChild();
#endif /* QT_VERSION */
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
#if QT_VERSION < 0x40000
           QListViewItem *next = firstChild();
#else /* QT_VERSION */
           Q3ListViewItem *next = firstChild();
#endif /* QT_VERSION */
           while ( next && ! (thatFound =((TQtObjectListItem *)next)->FindByName(objName)) ) 
              next = next->nextSibling();
        }
        return thatFound;
     }
};
#endif
#endif
