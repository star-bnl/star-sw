// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtIconBrowserImp.h,v 1.6 2013/08/30 16:00:21 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtIconBrowserImp
#define ROOT_TQtIconBrowserImp

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtIconBrowserImp                                                        //
//                                                                      //
// ABC describing GUI independent browser implementation protocol.      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TBrowserImp.h"
#include "TQtUpdateViewFlag.h"
#include "TQtGui.h"

#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
#  include <qptrvector.h>
#  include <qiconview.h>
#  include <qiconset.h>
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
#  include <q3ptrvector.h>
#  include <q3iconview.h>
#  include <qicon.h>
//MOC_SKIP_END
#endif /* QT_VERSION */
#include <qstring.h>

#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  class QListView;
  class QListViewItem;
  class QWidgetStack;
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  class Q3ListView;
  class Q3ListViewItem;
  class Q3WidgetStack;
//MOC_SKIP_END
#endif /* QT_VERSION */
class TQtBrowserItem;
class TBrowserCustom;

#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  class TQtIconBrowserItem : public QObject, public QIconViewItem {
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  class TQtIconBrowserItem : public QObject, public Q3IconViewItem {
//MOC_SKIP_END
#endif /* QT_VERSION */
    Q_OBJECT
protected:
  TObject *fObject;
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  const QIconSet *fIconSet;
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  const QIcon *fIconSet;
//MOC_SKIP_END
#endif /* QT_VERSION */
public:

#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  TQtIconBrowserItem( TObject *obj,QIconView * parent, QString label1) : QIconViewItem(parent,label1), fObject(obj) {;}
  TQtIconBrowserItem( TObject *obj,QIconView * parent, QIconViewItem * after ): QIconViewItem(parent,after ), fObject(obj){}
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  TQtIconBrowserItem( TObject *obj,Q3IconView * parent, QString label1) : Q3IconViewItem(parent,label1), fObject(obj) {;}
  TQtIconBrowserItem( TObject *obj,Q3IconView * parent, Q3IconViewItem * after ): Q3IconViewItem(parent,after ), fObject(obj){}
//MOC_SKIP_END
#endif /* QT_VERSION */
  void Browse(TBrowser *b){ if (fObject && b) fObject->Browse(b);}
  TObject *Object(){ return fObject;}
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  void SetIconSet(const QIconSet *set, QIconSet::Size size, QIconSet::Mode mode=QIconSet::Normal, QIconSet::State state=QIconSet::Off)
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  void SetIconSet(const QIcon *set, QIcon::Size size, QIcon::Mode mode=QIcon::Normal, QIcon::State state=QIcon::Off)
//MOC_SKIP_END
#endif /* QT_VERSION */
  {fIconSet = set; SetPixmap(size,mode,state); }
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  void SetPixmap(QIconSet::Size size, QIconSet::Mode mode=QIconSet::Normal, QIconSet::State state=QIconSet::Off)
  { QIconViewItem::setPixmap(fIconSet->pixmap(size,mode,state));}
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  void SetPixmap(QIcon::Size size, QIcon::Mode mode=QIcon::Normal, QIcon::State state=QIcon::Off)
  { Q3IconViewItem::setPixmap(fIconSet->pixmap(size,mode,state));}
//MOC_SKIP_END
#endif /* QT_VERSION */
};

class TQMimeTypes;
class TQtIconBrowserImp :  public QObject, public TBrowserImp {
  Q_OBJECT
    friend class ev;
protected:
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  QWidgetStack     *fBrowserImpID;
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  Q3WidgetStack     *fBrowserImpID;
//MOC_SKIP_END
#endif /* QT_VERSION */
  Int_t             fIconWidgetId;
  Int_t             fDetailWidgetID;
  Int_t             fX,fY;
  UInt_t            fWidth,fHeight;
  QString           fTitle;  
  Bool_t            fFolderExpanded;
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  QIconSet::Size    fIconSize;
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  QIcon::Size    fIconSize;
//MOC_SKIP_END
#endif /* QT_VERSION */
  TQtUpdateViewFlag fUpdate;
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  QListView        *fStackBrowser;
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  Q3ListView        *fStackBrowser;
//MOC_SKIP_END
#endif /* QT_VERSION */
  TQtBrowserItem   *fCurrentItem;
  TBrowserCustom   *fBrowserCustom;
  TObject          *fRootObject;
  TQtGui::TQtIconViewOptions fCurentViewMode;

  static TQMimeTypes *gfMimeTypeList;

  static void         CreateIcons();
public:

   enum EListOfIcons {kMainROOTIcon, kCanvasIcon, kBrowserIcon, kClosedFolderIcon, kOpenedFolderIcon,  kDocumentIcon, kTotalNumOfICons };

   static TQMimeTypes *IconList();
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
  static const QIconSet  *Shape2GeoShapeIcon(const char *shapeName);
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
   static const QIcon  *Shape2GeoShapeIcon(const char *shapeName);
//MOC_SKIP_END
#endif /* QT_VERSION */

   TQtIconBrowserImp(TBrowser *b=0,bool initFlag=true);
   TQtIconBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height,bool initFlag=true);
   TQtIconBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag=true);
   virtual ~TQtIconBrowserImp();

   TQtIconBrowserItem * Add(TObject *obj, const char *caption,const char *iconKey, Int_t check);

   // The base class overloaded methods:

   virtual void  Add(TObject *, const char *caption, Int_t check);
   virtual void  AddCheckBox(TObject *, Bool_t check);
   virtual void  CheckObjectItem(TObject *, Bool_t check);
   virtual void  RemoveCheckBox(TObject *);

   virtual void  Add(TObject *obj, const char *caption);
   virtual void  BrowseObj(TObject *);
   virtual void  ExecuteDefaultAction(TObject *);
   virtual Option_t *GetDrawOption() const;
   virtual void  Iconify();
   virtual void  RecursiveRemove(TObject *);
   virtual void  Refresh(Bool_t = kFALSE);
   virtual void  SetDrawOption(Option_t *option="");
   virtual void  Show();

   // TObject overloaded methods:
   const char   *GetTitle() const;
   QPaintDevice *GetBrowserID();
   // This class own data-members:
protected:
   // This class own methods:
   void   CreateStackBrowser();
   void   CreateDetailView();
   // void ResetPixmaps();

public:
   virtual Int_t InitWindow(Bool_t show=kFALSE);

public slots:
   void Add(const TQtBrowserItem *item);
   void BrowseObject(TObject *obj);
   void BrowseParentObject();
   void Chdir(const TQtBrowserItem *item);
   void Clear(TObject *);
   void FolderExpanded(TObject *obj, Bool_t expand=kTRUE);
   void EnableUpdates(Bool_t updt=kTRUE);
   void SetViewMode(int mode);
   void SetSortIndicator(int section);
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
   void SetIconSize(QIconSet::Size size);
   void StackClicked(QListViewItem *item);
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
   void SetIconSize(QIcon::Size size);
   void StackClicked(Q3ListViewItem *item);
//MOC_SKIP_END
#endif /* QT_VERSION */


protected slots:
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
   void ReplaceStack(const QPtrVector<TQtBrowserItem> &folderList);
   void ClickedItem(QIconViewItem *item);
   void PopMenu(QIconViewItem *item, const QPoint &pos);
   void SelectionChanged(QIconViewItem *item);
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
   void ReplaceStack(const Q3PtrVector<TQtBrowserItem> &folderList);
   void ClickedItem(Q3IconViewItem *item);
   void PopMenu(Q3IconViewItem *item, const QPoint &pos);
   void SelectionChanged(Q3IconViewItem *item);
//MOC_SKIP_END
#endif /* QT_VERSION */

signals:
   void ActivateObject(TObject *);
   void ActivateParent(TObject *);
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
   void StackHasBeenCreated(QListView *);
#endif
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
   void StackHasBeenCreated(Q3ListView *);
//MOC_SKIP_END
#endif /* QT_VERSION */
   void SwitchTreeView(Int_t flag=0);
   void ResetActionRequest(int viewMode);
};
inline  const char* TQtIconBrowserImp::GetTitle() const { return fTitle;        }
#if QT_VERSION < 0x40000
#ifndef Q_MOC_RUN
inline  void TQtIconBrowserImp::SetIconSize(QIconSet::Size size) { fIconSize = size;     }
#endif
#else /* QT_VERSION */
inline  void TQtIconBrowserImp::SetIconSize(QIcon::Size size) { fIconSize = size;     }
#endif /* QT_VERSION */

#endif
