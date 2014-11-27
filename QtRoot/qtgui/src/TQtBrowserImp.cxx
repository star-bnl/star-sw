// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtBrowserImp.cxx,v 1.10 2013/08/30 16:00:23 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtBrowserImp                                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtBrowserImp.h"
#include "TBrowserCustom.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TFile.h"
#include "TKey.h"
#include "TContextMenu.h"
#include "TBrowser.h"

#include "TQtObjectListItem.h"

#include "q3popupmenu.h"
#include <QMessageBox>
#include <QStatusBar>
#include <QLabel>
#include <QImage>
#include <QApplication>
#include "q3ptrstack.h"
#include "q3ptrvector.h"
#include "q3listview.h"
#include <QPixmap>
#include <QFileInfo>

static QPixmap *folderLocked = 0;
static QPixmap *folderClosed = 0;
static QPixmap *folderOpen = 0;
static QPixmap *fileNormal = 0;


static const char* folder_closed_xpm[]={
    "16 16 9 1",
    "g c #808080",
    "b c #c0c000",
    "e c #c0c0c0",
    "# c #000000",
    "c c #ffff00",
    ". c None",
    "a c #585858",
    "f c #a0a0a4",
    "d c #ffffff",
    "..###...........",
    ".#abc##.........",
    ".#daabc#####....",
    ".#ddeaabbccc#...",
    ".#dedeeabbbba...",
    ".#edeeeeaaaab#..",
    ".#deeeeeeefe#ba.",
    ".#eeeeeeefef#ba.",
    ".#eeeeeefeff#ba.",
    ".#eeeeefefff#ba.",
    ".##geefeffff#ba.",
    "...##gefffff#ba.",
    ".....##fffff#ba.",
    ".......##fff#b##",
    ".........##f#b##",
    "...........####."};

static const char* folder_open_xpm[]={
    "16 16 11 1",
    "# c #000000",
    "g c #c0c0c0",
    "e c #303030",
    "a c #ffa858",
    "b c #808080",
    "d c #a0a0a4",
    "f c #585858",
    "c c #ffdca8",
    "h c #dcdcdc",
    "i c #ffffff",
    ". c None",
    "....###.........",
    "....#ab##.......",
    "....#acab####...",
    "###.#acccccca#..",
    "#ddefaaaccccca#.",
    "#bdddbaaaacccab#",
    ".eddddbbaaaacab#",
    ".#bddggdbbaaaab#",
    "..edgdggggbbaab#",
    "..#bgggghghdaab#",
    "...ebhggghicfab#",
    "....#edhhiiidab#",
    "......#egiiicfb#",
    "........#egiibb#",
    "..........#egib#",
    "............#ee#"};

static const char * folder_locked[]={
    "16 16 10 1",
    "h c #808080",
    "b c #ffa858",
    "f c #c0c0c0",
    "e c #c05800",
    "# c #000000",
    "c c #ffdca8",
    ". c None",
    "a c #585858",
    "g c #a0a0a4",
    "d c #ffffff",
    "..#a#...........",
    ".#abc####.......",
    ".#daa#eee#......",
    ".#ddf#e##b#.....",
    ".#dfd#e#bcb##...",
    ".#fdccc#daaab#..",
    ".#dfbbbccgfg#ba.",
    ".#ffb#ebbfgg#ba.",
    ".#ffbbe#bggg#ba.",
    ".#fffbbebggg#ba.",
    ".##hf#ebbggg#ba.",
    "...###e#gggg#ba.",
    ".....#e#gggg#ba.",
    "......###ggg#b##",
    ".........##g#b##",
    "...........####."};

static const char * pix_file []={
    "16 16 7 1",
    "# c #000000",
    "b c #ffffff",
    "e c #000000",
    "d c #404000",
    "c c #c0c000",
    "a c #ffffc0",
    ". c None",
    "................",
    ".........#......",
    "......#.#a##....",
    ".....#b#bbba##..",
    "....#b#bbbabbb#.",
    "...#b#bba##bb#..",
    "..#b#abb#bb##...",
    ".#a#aab#bbbab##.",
    "#a#aaa#bcbbbbbb#",
    "#ccdc#bcbbcbbb#.",
    ".##c#bcbbcabb#..",
    "...#acbacbbbe...",
    "..#aaaacaba#....",
    "...##aaaaa#.....",
    ".....##aa#......",
    ".......##......."};


//______________________________________________________________________________
TQtBrowserImp::TQtBrowserImp(TBrowser *b,bool initFlag) : TBrowserImp(b),
  fBrowserImpID(0)
  ,fX(0),fY(0),fWidth(0),fHeight(0),fActiveItem(0),fRootItem(0)
  ,fRealFolder(kFALSE),fBrowserCustom(0)
{ 
   CreateIcons();
   fLocked = fSwitched = kFALSE;
   if (initFlag) InitWindow(); 
}
//______________________________________________________________________________
TQtBrowserImp::TQtBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height,bool initFlag)
: TBrowserImp(b)
 ,fBrowserImpID(0),fX(0),fY(0),fWidth(width),fHeight(height),fTitle(title),fActiveItem(0)
 ,fRootItem(0),fRealFolder(kFALSE), fBrowserCustom(0) 
{
   CreateIcons();
   fLocked = fSwitched = kFALSE;
   if (initFlag) InitWindow(); 
}
//______________________________________________________________________________
TQtBrowserImp::TQtBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag)
: TBrowserImp(b) 
 ,fBrowserImpID(0),fX(x),fY(y),fWidth(width),fHeight(height),fTitle(title),fActiveItem(0),fRootItem(0)
 ,fRealFolder(kFALSE), fBrowserCustom(0)
{
   CreateIcons();
   fLocked = fSwitched = kFALSE;
   if (initFlag)  InitWindow(); 
}
//______________________________________________________________________________
TQtBrowserImp::~TQtBrowserImp() { 
   TBrowserCustom *erase = fBrowserCustom; fBrowserCustom = 0;
   delete erase; 
}
//______________________________________________________________________________
void  TQtBrowserImp::CloseBranch(int depth)
{
   // close all open folders of the selected branch
  int size = fOpenFolderList.count();
  if (depth < size) 
         for (int i=depth;(i<size)&&(CloseItem(i));i++);
}
//______________________________________________________________________________
TQtBrowserItem *TQtBrowserImp::CloseItem(int depth)
{
   // paint the item with the "close" icon 
   // and remove it from the list if any
   // returns the pointer to the closed item
    TQtBrowserItem *item = fOpenFolderList.take(depth);
    if (item) item->setPixmap(0,*folderClosed);
    return item;
}
//______________________________________________________________________________
void TQtBrowserImp::Open(TQtBrowserItem *item,Bool_t chdir) 
{
   if (!item) return;
   if (chdir) {
      TObject *o = item->Object();
      if (o && 
             (   (o->IsA() == TKey::Class() ) 
              || (o->InheritsFrom(TDirectory::Class())) 
              ) 
         ) 
      {         
         TQtBrowserItem *i = (TQtBrowserItem *)item->
#if QT_VERSION < 0x40000
               QListViewItem::parent();
#else /* QT_VERSION */
                Q3ListViewItem::parent();
#endif /* QT_VERSION */
         Chdir(i);            
      } 
   }
   int depth = item->depth();
//   fprintf(stderr, " Open: depth = %d item %p %p\n", depth, item, fOpenFolderList[depth]);
   if (item != fOpenFolderList[depth]) { 
      CloseBranch(depth);
      item->setPixmap(0,*folderOpen);
      fOpenFolderList.insert(depth,item);
      fActiveItem = item;
      // open the parent folders also 
      TQtBrowserItem *oldItem = 0;
#if QT_VERSION < 0x40000
      while ((item = (TQtBrowserItem *)item->QListViewItem::parent()) 
#else /* QT_VERSION */
      while ((item = (TQtBrowserItem *)item->Q3ListViewItem::parent()) 
#endif /* QT_VERSION */
         && --depth 
         && item != oldItem) 
      {
         oldItem = fOpenFolderList[depth];
         if (oldItem)
            oldItem->setPixmap(0,*folderClosed);
         fOpenFolderList.insert(depth,item);
         item->setPixmap(0,*folderOpen);
      } 
   }
}
//______________________________________________________________________________
void TQtBrowserImp::Add(TObject *obj, const char *caption) 
{
   // Add object to iconbox. Class is used to get the associated icons
   // via the mime file (see GetObjPictures()).
   
   Add(obj,caption, -1);
}
//______________________________________________________________________________
void TQtBrowserImp::Add(TObject *obj, const char *caption, Int_t check)
{ 
   // Add items to the browser. This function has to be called
   // by the Browse() member function of objects when they are
   // called by a browser. If check < 0 (default) no check box is drawn,
   // if 0 then unchecked checkbox is added, if 1 checked checkbox is added.
   
  if (!fBrowserImpID) InitWindow();
   if (fLocked) return;

   const char *n = caption;
   if (!n) n = obj->GetName();
   if (QString("..") == QString(n) ) return;

   if (obj->IsFolder()) {
      // eliminate the second copy of the object
      bool isDirectory = fActiveItem
                      && (fActiveItem->Object()->IsA() == TKey::Class() )
                      && !(strcmp(((TKey*)fActiveItem->Object())->GetClassName(),"TDirectory"));
      if (fActiveItem  && !isDirectory
           && fActiveItem->DoesChildrenContain(obj)) return;
      TQtBrowserItem *item = 0;
      fRealFolder = kTRUE;
      if (fActiveItem) 
         item = new TQtBrowserItem(obj,fActiveItem,n,obj->ClassName());
      else if (fRootItem) {
           fRootItem->SetObject(obj);
           item = fRootItem;
      } else { 
         item = new TQtBrowserItem(obj,fBrowserImpID,n,obj->ClassName());
         item->SetExpanded(kFALSE);
         //fRootItem = item;
         //fActiveItem = fRootItem;
      }
      unsigned int depth = item->depth();
      // we do not need to shrink the vector
      if (depth >= fOpenFolderList.size()) fOpenFolderList.resize(depth+1) ; 
      item->setExpandable (true);
      item->setSelectable (true);

      item->setPixmap(0,*folderClosed);
      connect(item,SIGNAL(destroyed(QObject *)),this,SLOT(DisconnectItem(QObject *)));
      if (!fActiveItem) Open(item);
   }
}
//______________________________________________________________________________
void  TQtBrowserImp::AddCheckBox(TObject */*obj*/, Bool_t /*check*/)
{  
   // Add a checkbox in the TGListTreeItem corresponding to obj
   // and a checkmark on TGLVEntry if check = kTRUE.
}
//______________________________________________________________________________
void TQtBrowserImp::Chdir(const TQtBrowserItem *item)
{
   // Make object associated with item the current directory.

   if (item) {
      const TQtBrowserItem *i = item;
      TString dir;
      while (i) 
      { 
         TObject *obj = i->Object();
         if (obj) {
            if (obj->IsA() == TDirectory::Class()) {
               dir = "/" + dir;
               dir = obj->GetName() + dir;
            }
            if (obj->IsA() == TFile::Class()) {
               dir = ":/" + dir;
               dir = obj->GetName() + dir;
            }
            if (obj->IsA() == TKey::Class()) {
               if (strcmp(((TKey*)obj)->GetClassName(), "TDirectory") == 0) {
                  dir = "/" + dir;
                  dir = obj->GetName() + dir;
               }
            }
         }
         i = (TQtBrowserItem *)i->
#if QT_VERSION < 0x40000
               QListViewItem::parent();
#else /* QT_VERSION */
               Q3ListViewItem::parent();
#endif /* QT_VERSION */
      }
      if (gDirectory && dir.Length()) gDirectory->cd(dir.Data());
   }
}
//______________________________________________________________________________
void  TQtBrowserImp::CheckObjectItem(TObject *obj, Bool_t check)
{ 
   // Check / uncheck the TGListTreeItem corresponding to this
   // object and add a checkmark on TGLVEntry if check = kTRUE.
}
//______________________________________________________________________________
void  TQtBrowserImp::RemoveCheckBox(TObject *obj)
{
  // Remove checkbox from TGListTree and checkmark from TGListView.
}
//______________________________________________________________________________
void  TQtBrowserImp::SetDrawOption(Option_t *option)
{ 
   // Sets drawing option.
}
//______________________________________________________________________________
Option_t *TQtBrowserImp::GetDrawOption() const
{  
   // Returns drawing option
   return "";
}

//______________________________________________________________________________
TQtBrowserItem *TQtBrowserImp::Current() const
{ 
   // return the current list view item
   return (TQtBrowserItem *)fBrowserImpID->currentItem ();
}
//______________________________________________________________________________
void TQtBrowserImp::SwitchParent(Int_t flag) 
{
  switch (flag) {
  case 0: fLocked   = fSwitched = kFALSE;
    break;
  case 1: fSwitched = kTRUE;
    break;
  case 2: fLocked   = kTRUE;
    break;
  };
}
//______________________________________________________________________________
void TQtBrowserImp::DisconnectItem(QObject *obj) 
{
  // Disconnect the destroyed items
    TQtBrowserItem*item = (TQtBrowserItem*)obj;
    if (fActiveItem == item) fActiveItem = 0;
} 
//______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtBrowserImp::ClickedItem(QListViewItem *item) 
#else /* QT_VERSION */
void TQtBrowserImp::ClickedItem(Q3ListViewItem *item) 
#endif /* QT_VERSION */
{
   if (!item) return; // it is possible after "Expanded"
#if QT_VERSION < 0x40000
   if (fRootItem && ( item == (QListViewItem*) fRootItem ) && !fRootItem->isOpen() ) {
#else /* QT_VERSION */
   if (fRootItem && ( item == (Q3ListViewItem*) fRootItem ) && !fRootItem->isOpen() ) {
#endif /* QT_VERSION */
       fRootItem->setOpen (true);
       fRootItem->setEnabled(FALSE);
   }

   // if (item->depth() == 0) return;
   TQtBrowserItem *justClicked = (TQtBrowserItem*)item;
   // Adjust the label
   TObject *o  = justClicked->Object();
   if ( o->IsA() == TSystemDirectory::Class() && 
      (QString(o->GetName()) == "workdir") ) {
         justClicked->setText(0, ((TNamed *)o)->GetTitle());
   }
   // fprintf(stderr," ClickedItem item=%p Current = %p\n",justClicked,Current());

   // The critical section must be here - danger of the share access to TBrowser instance

   Browser()->SetSelected(justClicked->Object());

   if (justClicked !=fOpenFolderList[item->depth()] ) {
      Open(justClicked);
      if (justClicked->isOpen()) {
         // we should not browse it itself
         emit ItemOpen(justClicked->Object());
      } else {
         TObject *obj = justClicked->Object();
         emit OpenFolder(obj);
         Chdir(justClicked);
      }
   } 
}
//______________________________________________________________________________
void TQtBrowserImp::MakeParentActive(TObject *obj) {
#if 1
   // slot: to activate the parent of the item with the obj provided
   // ExpandedItem(fActiveItem);
   // fprintf(stderr," %s Activate the top directory \"%s\"\n",__FUNCTION__,obj->GetName());
   if ( strcmp(fActiveItem->Object()->GetName(), "workdir") ) {
       TQtBrowserItem *parentItem = (TQtBrowserItem *)fActiveItem->Parent();
       fActiveItem  = parentItem;
       CollapsedItem(fActiveItem);
   } else {
      // change the default path 
      //fprintf(stderr," 1. - %s Activate the top directory; new = \"%s\":\"%s\"\n",__FUNCTION__,((TNamed *)obj)->GetTitle()
      //   ,gSystem->WorkingDirectory());
      QString currentPath(((TNamed *)obj)->GetTitle());
      if (currentPath == "") currentPath = "/";
      QFileInfo path(currentPath); ((TNamed *)obj)->SetTitle((const char *)currentPath);
      fActiveItem->setText(0, path.absFilePath () ); 
      // ((TNamed *)obj)->SetTitle((const char *)path.dirPath(true));
      //fprintf(stderr," 2. - %s Activate the top directory; new = \"%s\":\"%s\"\n",__FUNCTION__,((TNamed *)obj)->GetTitle()
      //   ,gSystem->WorkingDirectory());
      CollapsedItem(fActiveItem);
   }
#else
   ChangeActive(obj,kTRUE);
#endif
}
//______________________________________________________________________________
void TQtBrowserImp::ChangeActive(TObject *obj) {
   // slot: to activate the item with the obj providee
   ChangeActive(obj,kFALSE);
}
//______________________________________________________________________________
void TQtBrowserImp::ChangeActive(TObject *obj,Bool_t /*parent*/) {
   // Look for the item containing the pointer to obj and make it active
   if (fActiveItem) {
      const TQtBrowserItem *found = 0;
      if (obj->IsA() == TSystemFile::Class() || obj->IsA() == TSystemDirectory::Class()) 
         found = (const TQtBrowserItem *)fActiveItem->FindByName(obj->GetName());
      else
         found = (const TQtBrowserItem *)fActiveItem->Find(obj);
      //fprintf(stderr," TQtBrowserImp::ChangeActive found %d, active=%p:%s; obj=%p:%s  \n"
      //   , found
      //   , fActiveItem->Object(), fActiveItem->Object()->GetName(), obj,obj->GetName() );
      if (!found) found = (TQtBrowserItem *)fBrowserImpID->firstChild();
      if (found) Open((TQtBrowserItem *)found);
   }
}

//______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtBrowserImp::ExpandedItem(QListViewItem *item) 
#else /* QT_VERSION */
void TQtBrowserImp::ExpandedItem(Q3ListViewItem *item) 
#endif /* QT_VERSION */
{
   TQtBrowserItem* justOpened = (TQtBrowserItem*)item;
   if (justOpened->Object() ) {
      // fprintf(stderr," ExpandedItem %p Current = %p\n",justOpened,Current());
      if (!justOpened->IsExpanded()) 
      {
         fActiveItem = justOpened;
         fRealFolder = kFALSE;
         // emit the current browser path
         emit CurrentPath(fOpenFolderList);
         // Browse() has a side effect to change the fRealFolder value
         // Block this widget update
      EnableUpdates(kFALSE);
      {
         // we have to freeze the update 
         justOpened->Browse(Browser());
      } 
      EnableUpdates(kTRUE);
      
         if (fRealFolder || ( justOpened->Object()->IsA() == TSystemDirectory::Class()) ) {
            fRealFolder = kFALSE;
            justOpened->SetExpanded(kTRUE);
         } else {
            justOpened->setExpandable(FALSE);
         }
      }
      // if (justOpened) emit FolderExpanded(justOpened->Object());
   }
}
//______________________________________________________________________________
void TQtBrowserImp::EnableUpdates(Bool_t updt)
{
   // Slot: Enable / disable the treeview update
   if (updt)  {
     fUpdate.UnFreezeToUpdate(fBrowserImpID);
     emit CanBeUpdated(updt);
   } else {
     emit CanBeUpdated(updt);
     fUpdate.FreezeToUpdate(fBrowserImpID); 
   }   
}
//______________________________________________________________________________
void TQtBrowserImp::CreateIcons()
{
   
  if ( !folderLocked ) {
    folderLocked = new QPixmap( folder_locked );
    folderClosed = new QPixmap( folder_closed_xpm );
    folderOpen   = new QPixmap( folder_open_xpm );
    fileNormal   = new QPixmap( pix_file );
  }
}
//______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtBrowserImp::PopMenu(QListViewItem *item, const QPoint &pos, int)
#else /* QT_VERSION */
void TQtBrowserImp::PopMenu(Q3ListViewItem *item, const QPoint &pos, int)
#endif /* QT_VERSION */
{
  TQtBrowserItem *that = (TQtBrowserItem*)item;
  TBrowser *b = Browser();
  TContextMenu *menu = b->GetContextMenu();
  if (that->Object() && menu)
    menu->Popup(pos.x(),pos.y(), that->Object(),b);
}
// ______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtBrowserImp::CollapsedItem(QListViewItem *item) 
#else /* QT_VERSION */
void TQtBrowserImp::CollapsedItem(Q3ListViewItem *item) 
#endif /* QT_VERSION */
{
   // Collapase item and remove all its children
   // Does this item "open"
   TQtBrowserItem *justCollapsed = (TQtBrowserItem *)item;
   unsigned int depth = justCollapsed->depth();
   bool needRedraw = false;
   if ( justCollapsed == fOpenFolderList[depth] ) {
      needRedraw = (depth < fOpenFolderList.count()-1);
      CloseBranch(depth);
   }
#if QT_VERSION < 0x40000
   QPtrStack<QListViewItem> garbageCollector;
#else /* QT_VERSION */
   Q3PtrStack<Q3ListViewItem> garbageCollector;
#endif /* QT_VERSION */
   garbageCollector.setAutoDelete(true);
#if QT_VERSION < 0x40000
   QListViewItem *waste = item->firstChild();
#else /* QT_VERSION */
   Q3ListViewItem *waste = item->firstChild();
#endif /* QT_VERSION */
   if (waste) {
      garbageCollector.push(waste);
      while( (waste = waste->nextSibling ()) ) {
         garbageCollector.push(waste);
      }
   }
   justCollapsed->SetExpanded(kFALSE);
   if (needRedraw ) {
      Open(justCollapsed);
      // make the current one 
      emit ItemOpen(justCollapsed->Object());
   }
}
//______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtBrowserImp::SelectionChanged(QListViewItem * /*item*/) 
#else /* QT_VERSION */
void TQtBrowserImp::SelectionChanged(Q3ListViewItem * /*item*/) 
#endif /* QT_VERSION */
{  
  //fprintf(stderr," Selection changed\n"); 
}
//______________________________________________________________________________
void TQtBrowserImp::BrowseObj(TObject *obj) 
{ 
   if (obj) {
     if (!obj->IsEqual(gROOT))  Add(obj,0);
#ifndef OLD
      TBrowser *b = 0;
      b = Browser();
      if (b) obj->Browse(b); 
#else
      if (fBrowserCustom) {
//          Emit("BrowseObj(TObject*)", (Long_t)obj);
          obj->Browse(fBrowserCustom);
      }
#endif
      fBrowser->SetRefreshFlag(kFALSE);
   }
}
//______________________________________________________________________________
void TQtBrowserImp::ExecuteDefaultAction(TObject * /*obj*/) { }
//______________________________________________________________________________
void TQtBrowserImp::Iconify() { if (fBrowserImpID) fBrowserImpID->hide(); }
//______________________________________________________________________________
void TQtBrowserImp::RecursiveRemove(TObject *obj)
{
   TQtBrowserItem *item = (TQtBrowserItem *)fBrowserImpID->firstChild();
   if (item) {
      TQtBrowserItem *toBeDeletedLast = (TQtBrowserItem *) item->RecursiveDelete(obj);
      delete toBeDeletedLast;
   }
}
//______________________________________________________________________________
void TQtBrowserImp::Refresh(Bool_t /*flag*/) { }
//______________________________________________________________________________
void TQtBrowserImp::Show() { 
   if (fBrowserImpID) {
      // fRootItem->setOpen (true);
      // fRootItem->setEnabled(FALSE);
      fBrowserImpID->show();
   }
}
//______________________________________________________________________________
Int_t TQtBrowserImp::InitWindow(Bool_t show)
{
   // Create a fake custom browser
   fBrowserCustom  = (TBrowserCustom *)TBrowserCustom::Class()->New();
   if (fBrowserCustom) fBrowserCustom->SetBrowserImp(this);

#if QT_VERSION < 0x40000
   fBrowserImpID = new QListView(0,"RootBrowser",Qt::WDestructiveClose);
#else /* QT_VERSION */
   fBrowserImpID = new Q3ListView(0,"RootBrowser",Qt::WDestructiveClose);
#endif /* QT_VERSION */
   fBrowserImpID->setCaption(fTitle);

   if (fX*fY) fBrowserImpID->setGeometry(fX,fY,fWidth,fHeight);
   else fBrowserImpID->resize(fWidth,fHeight);

#if QT_VERSION < 0x40000
   connect(fBrowserImpID, SIGNAL(clicked ( QListViewItem *))
      ,this, SLOT(ClickedItem(QListViewItem*)));
#else /* QT_VERSION */
   connect(fBrowserImpID, SIGNAL(clicked ( Q3ListViewItem *))
      ,this, SLOT(ClickedItem(Q3ListViewItem*)));
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(fBrowserImpID, SIGNAL(collapsed ( QListViewItem *))
      ,this, SLOT(CollapsedItem(QListViewItem*)));
#else /* QT_VERSION */
   connect(fBrowserImpID, SIGNAL(collapsed ( Q3ListViewItem *))
      ,this, SLOT(CollapsedItem(Q3ListViewItem*)));
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(fBrowserImpID, SIGNAL(expanded ( QListViewItem *))
      ,this, SLOT(ExpandedItem(QListViewItem*)));
#else /* QT_VERSION */
   connect(fBrowserImpID, SIGNAL(expanded ( Q3ListViewItem *))
      ,this, SLOT(ExpandedItem(Q3ListViewItem*)));
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(fBrowserImpID, SIGNAL(selectionChanged ( QListViewItem * ))
      , this, SLOT(SelectionChanged(QListViewItem *))); 
#else /* QT_VERSION */
   connect(fBrowserImpID, SIGNAL(selectionChanged ( Q3ListViewItem * ))
      , this, SLOT(SelectionChanged(Q3ListViewItem *))); 
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(fBrowserImpID, SIGNAL(rightButtonPressed( QListViewItem *, const QPoint &, int ))
      ,this, SLOT(PopMenu(QListViewItem *, const QPoint &, int))); 
#else /* QT_VERSION */
   connect(fBrowserImpID, SIGNAL(rightButtonPressed( Q3ListViewItem *, const QPoint &, int ))
      ,this, SLOT(PopMenu(Q3ListViewItem *, const QPoint &, int))); 
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(fBrowserImpID, SIGNAL(contextMenuRequested( QListViewItem *, const QPoint &, int ))
      ,this, SLOT(PopMenu(QListViewItem *, const QPoint &, int))); 
#else /* QT_VERSION */
   connect(fBrowserImpID, SIGNAL(contextMenuRequested( Q3ListViewItem *, const QPoint &, int ))
      ,this, SLOT(PopMenu(Q3ListViewItem *, const QPoint &, int))); 
#endif /* QT_VERSION */

   fBrowserImpID->addColumn("ROOT folders");
   fBrowserImpID->addColumn("class name");
#ifdef SORT_ROW_BY_DEFAULT   
   fBrowserImpID->setShowSortIndicator(true); 
#endif   
   fRootItem = new TQtBrowserItem(0,fBrowserImpID,"ROOT");
   Add(gROOT,"ROOT");
   fRootItem->setEnabled(true);
   fRootItem->setOpen (FALSE);
   // fRootItem->setEnabled(FALSE);
   // fRootItem->setOpen (true);
   
   if (show) Show();
   return 0;
}
