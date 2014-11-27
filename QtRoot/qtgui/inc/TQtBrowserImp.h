// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtBrowserImp.h,v 1.6 2013/08/30 16:00:20 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtBrowserImp
#define ROOT_TQtBrowserImp

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtBrowserImp                                                        //
//                                                                      //
// ABC describing GUI independent browser implementation protocol.      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TBrowserImp.h"
#include "TSystemDirectory.h"
#include "TClass.h"

#include "TQtObjectListItem.h"
#include "TQtUpdateViewFlag.h"

#include <QtGlobal>
#include <QString>

#include <q3ptrvector.h>
#include <q3listview.h>


// class TQtBrowserWidget;

class TBrowserCustom;
class TQtBrowserItem;
// class QPtrVector<TQtBrowserItem>;

class TQtBrowserImp :  public QObject, public TBrowserImp {
  Q_OBJECT
    friend class ev;
protected:
#if QT_VERSION < 0x40000
  QListView  *fBrowserImpID;
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  Q3ListView  *fBrowserImpID;
//MOC_SKIP_END
#endif /* QT_VERSION */
  TBrowserImp  *fParent;
  Int_t fX,fY;
  UInt_t fWidth,fHeight;
  QString fTitle;
  Bool_t  fLocked;
  Bool_t  fSwitched;
  
private:
  TQtBrowserItem   *fActiveItem;
  TQtBrowserItem   *fRootItem;
  Bool_t            fRealFolder;

#if QT_VERSION < 0x40000
  QPtrVector<TQtBrowserItem> fOpenFolderList;
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  Q3PtrVector<TQtBrowserItem> fOpenFolderList;
//MOC_SKIP_END
#endif /* QT_VERSION */

  TQtUpdateViewFlag fUpdate;

protected:  
  TBrowserCustom    *fBrowserCustom;

  static void CreateIcons();

public:

   enum EListOfIcons {kMainROOTIcon, kCanvasIcon, kBrowserIcon, kClosedFolderIcon, kOpenedFolderIcon,  kDocumentIcon, kTotalNumOfICons };

   TQtBrowserImp(TBrowser *b=0,bool initFlag=true);
   TQtBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height,bool initFlag=true);
   TQtBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag=true);
   TQtBrowserImp(TBrowserImp *parent);
   virtual ~TQtBrowserImp();

  // The base class overloaded methods:

   virtual void  Add(TObject *, const char *caption, Int_t);
   virtual void  Add(TObject *, const char *caption);
   virtual void  AddCheckBox(TObject *, Bool_t = kFALSE);
   virtual void  CheckObjectItem(TObject *, Bool_t = kFALSE);
   virtual void  RemoveCheckBox(TObject *);
   virtual void  BrowseObj(TObject *);
   virtual void  ExecuteDefaultAction(TObject *);
   virtual void  Iconify();
   virtual void  RecursiveRemove(TObject *);
   virtual void  Refresh(Bool_t = kFALSE);
   virtual void  SetDrawOption(Option_t *option="");
   virtual Option_t *GetDrawOption() const;
   virtual void  Show();

   // TObject overloaded methods:
   const char* GetTitle() const;
   QPaintDevice *GetBrowserID(){ return fBrowserImpID; }
   // This class own data-members:
protected:
   // This class own methods:
   void            ChangeActive(TObject *obj,Bool_t parent);
   void            Chdir(const TQtBrowserItem *item);
   void            CloseBranch(int depth);
   TQtBrowserItem *CloseItem(int depth);
   void            Open(TQtBrowserItem *item, Bool_t chdir=kFALSE);

public:
   virtual Int_t InitWindow(Bool_t show=kFALSE);
   virtual TQtBrowserItem *Current() const;

public slots:
   void SwitchParent(Int_t flag);
   void ChangeActive(TObject *obj);
   void MakeParentActive(TObject *obj);
   void EnableUpdates(Bool_t updt=kTRUE);

protected slots:
#if QT_VERSION < 0x40000
  void ClickedItem(QListViewItem *item);
  void CollapsedItem(QListViewItem *item);
  void ExpandedItem(QListViewItem *item);
  void PopMenu(QListViewItem *item, const QPoint &pos, int id);
  void SelectionChanged(QListViewItem *item);
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  void ClickedItem(Q3ListViewItem *item);
  void CollapsedItem(Q3ListViewItem *item);
  void ExpandedItem(Q3ListViewItem *item);
  void PopMenu(Q3ListViewItem *item, const QPoint &pos, int id);
  void SelectionChanged(Q3ListViewItem *item);
//MOC_SKIP_END
#endif /* QT_VERSION */
  void DisconnectItem(QObject *obj);

signals:
  void CanBeUpdated(Bool_t);
#if QT_VERSION < 0x40000
  void CurrentPath(const QPtrVector<TQtBrowserItem> &);
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  void CurrentPath(const Q3PtrVector<TQtBrowserItem> &);
//MOC_SKIP_END
#endif /* QT_VERSION */
  void FolderExpanded(TObject *obj, Bool_t expand=kTRUE);
  void ItemOpen(TObject *);
  void OpenFolder(TObject *);
};
inline  const char* TQtBrowserImp::GetTitle() const {return fTitle;}

#endif
