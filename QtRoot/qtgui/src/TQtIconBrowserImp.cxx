// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtIconBrowserImp.cxx,v 1.6 2013/08/30 16:00:24 perev Exp $
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
// TQtIconBrowserImp                                                    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtIconBrowserImp.h"
#include "TQtBrowserImp.h"
#include "TQtRootBrowserAction.h"
#include "TQtGui.h"
#ifndef OLD
#include "TBrowserCustom.h"
#endif

#include "TQtObjectListItem.h"

// #define ATLASTABLEBROWSER

#include "TROOT.h"
#include "TContextMenu.h"
#include "TBrowser.h"
#include "TSystem.h"
#include "TFile.h"
#include "TSystemFile.h"
#include "TEnv.h"
#include "TKey.h"

#ifdef ATLASTABLEBROWSER
#  include "TTable.h"
#  include "TColumnView.h"
#endif

#include "TQMimeTypes.h"

#if QT_VERSION < 0x40000
#include <qpopupmenu.h>
#else /* QT_VERSION */
#include <q3popupmenu.h>
#endif /* QT_VERSION */
#include <qlabel.h>
#if QT_VERSION < 0x40000
#include <qlistview.h>
#include <qtable.h>
#include <qwidgetstack.h>
#else /* QT_VERSION */
#include <q3listview.h>
#include <q3table.h>
#include <q3widgetstack.h>
#endif /* QT_VERSION */
#include <qpixmap.h>

#if QT_VERSION < 0x40000
static QIconSet *folderClosed = 0;
static QIconSet *fileNormal = 0;
#else /* QT_VERSION */
static QIcon *folderClosed = 0;
static QIcon *fileNormal = 0;
#endif /* QT_VERSION */

static const char* folder_closed_xpm[]={
   "32 32 11 1",
    "# c #000000",
    "b c #c0c000",
    "d c #585858",
    "a c #ffff00",
    "i c #400000",
    "h c #a0a0a4",
    "e c #000000",
    "c c #ffffff",
    "f c #303030",
    "g c #c0c0c0",
    ". c None",
    "...###..........................",
    "...#aa##........................",
    ".###baaa##......................",
    ".#cde#baaa##....................",
    ".#cccdeebaaa##..##f.............",
    ".#cccccdeebaaa##aaa##...........",
    ".#cccccccdeebaaaaaaaa##.........",
    ".#cccccccccdeebababaaa#.........",
    ".#cccccgcgghhebbbbbbbaa#........",
    ".#ccccccgcgggdebbbbbbba#........",
    ".#cccgcgcgcgghdeebiebbba#.......",
    ".#ccccgcggggggghdeddeeba#.......",
    ".#cgcgcgcggggggggghghdebb#......",
    ".#ccgcggggggggghghghghd#b#......",
    ".#cgcgcggggggggghghghhd#b#......",
    ".#gcggggggggghghghhhhhd#b#......",
    ".#cgcggggggggghghghhhhd#b#......",
    ".#ggggggggghghghhhhhhhdib#......",
    ".#gggggggggghghghhhhhhd#b#......",
    ".#hhggggghghghhhhhhhhhd#b#......",
    ".#ddhhgggghghghhhhhhhhd#b#......",
    "..##ddhhghghhhhhhhhhhhdeb#......",
    "....##ddhhhghhhhhhhhhhd#b#......",
    "......##ddhhhhhhhhhhhhd#b#......",
    "........##ddhhhhhhhhhhd#b#......",
    "..........##ddhhhhhhhhd#b#......",
    "............##ddhhhhhhd#b###....",
    "..............##ddhhhhd#b#####..",
    "................##ddhhd#b######.",
    "..................##dddeb#####..",
    "....................##d#b###....",
    "......................####......"};


static const char * pix_file []={
    "32 32 17 1",
    "# c #000000",
    "a c #ffffff",
    "j c #808080",
    "n c #a0a0a4",
    "g c #c0c0c0",
    "m c #004000",
    "o c #000000",
    "l c #004040",
    "k c #404000",
    "i c #c0c000",
    "h c #ffff00",
    "b c #ffffc0",
    "e c #ff8000",
    "f c #c05800",
    "c c #ffa858",
    "d c #ffdca8",
    ". c None",
    "................................",
    "................................",
    "................................",
    "................................",
    ".............#....###...........",
    "...###......#a##.#aba##.........",
    "..#cdb#....#aaaa#aaaaaa##.......",
    "..#ecdb#..#aaaa#aaaaaaaba##.....",
    "..#fecdb##aaaa#aaaaaaaaaaab##...",
    "...#fecdb#aaa#aaaaaaabaabaaaa##.",
    "....#fecdb#a#baaaaa#baaaaaabaaa#",
    ".....#fecdb#aaaaab#a##baaaaaaa#.",
    ".....##fecdb#bbba#aaaa##baaab#..",
    "....#bb#fecdb#ba#aaaaaaa##aa#...",
    "...#bbbb#fecdb##aaabaaaaaa##....",
    "..#bbbb#b#fecdb#aaaaaaabaaaa##..",
    ".#bbbb#bbb#fecdg#aaaaaaaaaaaba#.",
    "#hhbb#bbbbb#fegg#iiaaaaaaaaaaaa#",
    "#jhhhklibbbk#ggj#aaiiaaaaaaaaa#j",
    ".#mjhhhkmikab####aaabiiaaaaaa#j.",
    "...##jhhhmaaibbaaiibaaaiiaab#n..",
    ".....##j#baaaiiabaaiibaabaa#n...",
    "......##baibaabiibaaaiiabb#j....",
    "......#bbbbiiaabbiiaaaaabon.....",
    ".....#bbbbbbbiiabbaiiaab#n......",
    ".....#jbbbbbbbbiibaabba#n.......",
    "......##jbbbbbbbbiiaabmj........",
    "........##jbbbbbbbbbb#j.........",
    "..........##nbbbbbbbmj..........",
    "............##jbbbb#j...........",
    "..............#mjj#n............",
    "................##n............."};

TQMimeTypes *TQtIconBrowserImp::gfMimeTypeList=0;

//______________________________________________________________________________
static void ArrayLayout(UInt_t *layout,const UInt_t *size, Int_t dim)
{
  // ************************************************************
  // *****  This method is temporary here. It must be moved to the 
  // *****  TTableDescriptor class
  // ************************************************************
  // ArrayLayout - calculates the array layout recursively
  //
  // Input:
  // -----
  // dim   - dimension of the targeted array
  // size  - the max index for each dimension
  //
  // Output:
  // ------
  // layout - the "start index" for each dimension of an array
  //

  if (dim && layout && size) {
    if (++layout[dim-1] >= size[dim-1]) {
        layout[dim-1] = 0;
        dim--;
        ArrayLayout(layout,size, dim);
    }
  }
}
//______________________________________________________________________________
TQtIconBrowserImp::TQtIconBrowserImp(TBrowser *b,bool initFlag) : TBrowserImp(b)
 , fBrowserImpID(0),fIconWidgetId(-1),fDetailWidgetID(-1)
 , fX(0),fY(0),fWidth(0),fHeight(0),fStackBrowser(0),fCurrentItem(0),fBrowserCustom(0)
 , fCurentViewMode(TQtGui::kNotDefinedYet)
 { 
  fFolderExpanded = kFALSE;
  CreateIcons();
  IconList();
  if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtIconBrowserImp::TQtIconBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height,bool initFlag)
:  TBrowserImp(b), fBrowserImpID(0),fIconWidgetId(-1),fDetailWidgetID(-1)
  ,fX(0),fY(0),fWidth(width),fHeight(height),fTitle(title)
#if QT_VERSION < 0x40000
  ,fIconSize(QIconSet::Large),fStackBrowser(0),fCurrentItem(0),fBrowserCustom(0)
#else /* QT_VERSION */
  ,fIconSize(QIcon::Large),fStackBrowser(0),fCurrentItem(0),fBrowserCustom(0)
#endif /* QT_VERSION */
 , fCurentViewMode(TQtGui::kNotDefinedYet)
{
  fFolderExpanded = kFALSE;
  CreateIcons();
  IconList();
  if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtIconBrowserImp::TQtIconBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag)
: TBrowserImp(b),fBrowserImpID(0),fIconWidgetId(-1),fDetailWidgetID(-1)
, fX(x),fY(y),fWidth(width),fHeight(height),fTitle(title)
#if QT_VERSION < 0x40000
, fIconSize(QIconSet::Large),fStackBrowser(0),fCurrentItem(0),fBrowserCustom(0)
#else /* QT_VERSION */
, fIconSize(QIcon::Large),fStackBrowser(0),fCurrentItem(0),fBrowserCustom(0)
#endif /* QT_VERSION */
, fCurentViewMode(TQtGui::kNotDefinedYet)
{
  fFolderExpanded = kFALSE;
  CreateIcons();
  IconList();
  if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtIconBrowserImp::~TQtIconBrowserImp() {
   if ( fStackBrowser && !fStackBrowser->parentWidget()) {
      // Save delete
#if QT_VERSION < 0x40000
      QListView *temp = fStackBrowser; fStackBrowser =0;
#else /* QT_VERSION */
      Q3ListView *temp = fStackBrowser; fStackBrowser =0;
#endif /* QT_VERSION */
      delete temp;
   }
   TBrowserCustom *erase = fBrowserCustom; fBrowserCustom = 0;
   delete erase; 
}
//______________________________________________________________________________
void TQtIconBrowserImp::Add(TObject *obj, const char *caption)
{
   // Add object to iconbox. Class is used to get the associated icons
   // via the mime file (see GetObjPictures()).
   
   Add(obj,caption, -1);
}
//______________________________________________________________________________
void TQtIconBrowserImp::Add(TObject *obj, const char *caption, Int_t /*check*/ ) 
{
   // Add object with icon image defined by the class name from the
   // $ROOTSYS/etc/root.mimes file
   
   // Add items to the browser. This function has to be called
   // by the Browse() member function of objects when they are
   // called by a browser. If check < 0 (default) no check box is drawn,
   // if 0 then unchecked checkbox is added, if 1 checked checkbox is added.
   
   TQtIconBrowserItem *item = Add(obj,caption,0, -1);
   if (item) {} // to suspend the compilation warning
   //if (!caption) {
   //   TQtBrowserItem save(obj,(QListView *)0,obj->GetName());
   //   Add(&save);
   //}
}
//______________________________________________________________________________
#if QT_VERSION < 0x40000
const QIconSet  *TQtIconBrowserImp::Shape2GeoShapeIcon(const char *shapeName) 
#else /* QT_VERSION */
const QIcon  *TQtIconBrowserImp::Shape2GeoShapeIcon(const char *shapeName) 
#endif /* QT_VERSION */
{
   /* 
  pattern = ";   TBRIK
  pattern = TGeoCone    TCONE
  pattern = TGeoConeSeg TCONS
  pattern = TGeoCtub    TCTUB 
  pattern = TGeoEltu    TELTU 
  pattern = TGeoXtru    TXTRU 
  pattern = TGeoHype    THYPE
  pattern = TGeoPcon    TPCON 
  pattern = TGeoPgon    TPGON 
  pattern = TGeoPara    TPARA
  pattern = TGeoSphere  TSPHE 
  pattern = TGeoTrd1    TTRD1
  pattern = TGeoTrd2    TTRD2 
  pattern = TGeoTrap    TTRAP
  pattern = TGeoTube    TTUBE 
  pattern = TGeoTubeSeg TTUBS
  pattern = TGeoGtra    TGTRA
   */ 
   QString shapeIconName = "TGeo";
   QString  shapeClass =  shapeName;
   shapeClass.remove(0,1);             // remove the leading "T"
   shapeIconName += shapeClass.ref(0); // pick the fist upper case shale name letter
   shapeClass.remove(0,1);             // Remove it
   shapeIconName += shapeClass.lower();// Convert the tail to the lower case and append it
   // exceptions
   if   (shapeIconName == "TGeoBrik") shapeIconName = "TGeoBBox";
   else if (shapeIconName == "TGeoCons") shapeIconName = "TGeoConeSeg";
   else if (shapeIconName == "TGeoTubs") shapeIconName = "TGeoTubeSeg";
   else if (shapeIconName == "TGeoSphe") shapeIconName = "TGeoSphere";
   // printf(" Final Form = %s\n", (const char *)shapeIconName.toLatin1().data());
   return IconList()->GetIcon(shapeIconName);
}
//______________________________________________________________________________
TQtIconBrowserItem *TQtIconBrowserImp::Add(TObject *obj, const char *caption,const char *iconKey, Int_t /*check*/)
{ 
   if (!obj) return 0;
   if (!fBrowserImpID) InitWindow();
   if (fFolderExpanded) return 0;
#if QT_VERSION < 0x40000
   QIconView *iconView = (QIconView *)fBrowserImpID->widget(fIconWidgetId);
#else /* QT_VERSION */
   Q3IconView *iconView = (Q3IconView *)fBrowserImpID->widget(fIconWidgetId);
#endif /* QT_VERSION */
   TQtIconBrowserItem *item = 0;
   // Check duplication
#if QT_VERSION < 0x40000
   QIconViewItem *nextItem;
#else /* QT_VERSION */
   Q3IconViewItem *nextItem;
#endif /* QT_VERSION */
   for ( nextItem = iconView->firstItem(); nextItem; nextItem = nextItem->nextItem() )
   {
      TObject *itemObj = ((TQtIconBrowserItem*)nextItem)->Object();
      if (itemObj) {
         if (itemObj->IsEqual(obj)                                            )  return (TQtIconBrowserItem *)nextItem; 
         if (itemObj->InheritsFrom(TNamed::Class()) && !itemObj->Compare(obj) )  return (TQtIconBrowserItem *)nextItem;
      }
   }
   // No copy has been found, it is safe to add one
   QString name = caption;
   if (!caption || !caption[0]) name = obj->GetName();
   else name = caption;
   TKey * isKey = (obj->IsA() == TKey::Class()) ? (TKey *)obj : 0;
   if (isKey) {
      name += ";";
      name +=  isKey->GetCycle();
   }
   item = new TQtIconBrowserItem(obj,iconView,name);
   item->setSelectable (true);
   if (obj->IsFolder()) {
      item->SetIconSet(folderClosed,fIconSize);
   } else {
      // check the ROOT icon
      //    TKey * isKey = dynamic_cast<TKey *>(obj);
#if QT_VERSION < 0x40000
      const QIconSet  *set = 0;
#else /* QT_VERSION */
      const QIcon  *set = 0;
#endif /* QT_VERSION */
      if  ( obj->InheritsFrom("TShape")  )
            set = Shape2GeoShapeIcon(obj->ClassName());
      if (!set) if (iconKey && iconKey[0]) set = gfMimeTypeList->GetIcon(iconKey);
      if (!set)  set = gfMimeTypeList->GetIcon(obj->GetIconName());
      if (!set) {
         if (isKey) set = gfMimeTypeList->GetIcon(isKey->GetClassName());
         else if  ( obj->IsA() == TSystemFile::Class() )  
            set = gfMimeTypeList->GetIcon((TSystemFile *)obj);
          else
            set = gfMimeTypeList->GetIcon(obj->ClassName());
      }
      if (set) 
         item->SetIconSet(set,fIconSize);
      else  
         item->SetIconSet(fileNormal,fIconSize);
   }
   return item;
}
//______________________________________________________________________________
void  TQtIconBrowserImp::AddCheckBox(TObject * /*obj*/, Bool_t /*check*/ )
{  
   // Add a checkbox in the TGListTreeItem corresponding to obj
   // and a checkmark on TGLVEntry if check = kTRUE.
}
//______________________________________________________________________________
void  TQtIconBrowserImp::CheckObjectItem(TObject *  /*obj*/, Bool_t /*check*/ )
{ 
   // Check / uncheck the TGListTreeItem corresponding to this
   // object and add a checkmark on TGLVEntry if check = kTRUE.
}
//______________________________________________________________________________
void  TQtIconBrowserImp::RemoveCheckBox(TObject *  /*obj*/ )
{
  // Remove checkbox from TGListTree and checkmark from TGListView.
}
//______________________________________________________________________________
void   TQtIconBrowserImp::SetDrawOption(Option_t * /*option*/ )
{ 
   // Sets drawing option.
}
//______________________________________________________________________________
Option_t * TQtIconBrowserImp::GetDrawOption() const
{  
   // Returns drawing option
   return "";
}
//__ Slot: _____________________________________________________________________
#if QT_VERSION < 0x40000
void TQtIconBrowserImp::ReplaceStack(const QPtrVector<TQtBrowserItem> &folderList)
#else /* QT_VERSION */
void TQtIconBrowserImp::ReplaceStack(const Q3PtrVector<TQtBrowserItem> &folderList)
#endif /* QT_VERSION */
{
   const TQtBrowserItem *item = 0;
   int nCount = folderList.count();
   if (nCount > 0) {
      item = folderList[0]; 
      if (fStackBrowser)  delete  fStackBrowser->firstChild(); // Clear the existing stack         
      else                CreateStackBrowser();
      fCurrentItem  = 0;
      for (int i=0; i < nCount;i++)  Add(folderList[i]);
   }
}
//_____________________________________________________________________________
void TQtIconBrowserImp::CreateStackBrowser()
{
   if (!fStackBrowser) {
      // create pseudo widget (no parent and hiden)
#if QT_VERSION < 0x40000
      fStackBrowser = new QListView();
#else /* QT_VERSION */
      fStackBrowser = new Q3ListView();
#endif /* QT_VERSION */
      fStackBrowser->hide();
#if QT_VERSION < 0x40000
      connect(fStackBrowser,SIGNAL(clicked(QListViewItem *)), this,SLOT(StackClicked(QListViewItem *)));
#else /* QT_VERSION */
      connect(fStackBrowser,SIGNAL(clicked(Q3ListViewItem *)), this,SLOT(StackClicked(Q3ListViewItem *)));
#endif /* QT_VERSION */
      emit StackHasBeenCreated(fStackBrowser);
   } 
}
//__ Slot: _____________________________________________________________________
void TQtIconBrowserImp::Add(const TQtBrowserItem *item)
{
   //  // copy ctor with the new parent if provided
   CreateStackBrowser();
   fCurrentItem  = fCurrentItem ? new TQtBrowserItem(item->Object(),fCurrentItem, item->text(1),item->text(2))
                                : new TQtBrowserItem(item->Object(),fStackBrowser,item->text(1),item->text(2));
   // add the item attributes:
   int column =0;
   fCurrentItem->setPixmap(column,*item->pixmap(column));
   fCurrentItem->setOpen(item->isOpen());
}
//__ Slot: _____________________________________________________________________
#if QT_VERSION < 0x40000
void TQtIconBrowserImp::StackClicked(QListViewItem *item)
#else /* QT_VERSION */
void TQtIconBrowserImp::StackClicked(Q3ListViewItem *item)
#endif /* QT_VERSION */
{
   fCurrentItem = (TQtBrowserItem *)item;
   delete fCurrentItem->firstChild ();
   if (fCurrentItem) 
      BrowseObject(fCurrentItem->Object());
}
//______________________________________________________________________________
void TQtIconBrowserImp::Clear(TObject *obj)
{
   // set the default view first
   emit ResetActionRequest(int(TQtGui::kViewSmallIcons));
   if (fBrowserImpID) {
#if QT_VERSION < 0x40000
      ((QIconView *)fBrowserImpID->widget(fIconWidgetId))->clear();
#else /* QT_VERSION */
      ((Q3IconView *)fBrowserImpID->widget(fIconWidgetId))->clear();
#endif /* QT_VERSION */
      if (fDetailWidgetID != -1) {
         fBrowserImpID->raiseWidget (fIconWidgetId);
         QWidget *details = fBrowserImpID->widget(fDetailWidgetID);
         fBrowserImpID->removeWidget(details);
         fDetailWidgetID  = -1;
         delete details;
      }
      FolderExpanded(obj,kFALSE);
      //   Add(obj);
   }
}
//__ Slot: ______________________________________________________________________
void TQtIconBrowserImp::BrowseParentObject()
{
#if QT_VERSION < 0x40000
   StackClicked(fCurrentItem->QListViewItem::parent());
#else /* QT_VERSION */
   StackClicked(fCurrentItem->Q3ListViewItem::parent());
#endif /* QT_VERSION */
}
//______________________________________________________________________________
void TQtIconBrowserImp::BrowseObject(TObject *obj)
{
  if (obj) {
    if (obj->IsFolder()) Clear(obj);
    emit ActivateObject(obj);
#ifdef OLD
    emit SwitchTreeView(2); // lock Tree view
    obj->Browse(Browser());
    emit SwitchTreeView(); // restore the normal flow in TreeView
#else
    if (fBrowserCustom) {
       obj->Browse(fBrowserCustom);
    }
#endif
    FolderExpanded(obj);
  }
}
//______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtIconBrowserImp::ClickedItem(QIconViewItem *item) 
#else /* QT_VERSION */
void TQtIconBrowserImp::ClickedItem(Q3IconViewItem *item) 
#endif /* QT_VERSION */
{
    //fprintf(stderr," ExpandedItem\n");
  if (!item)  return;

  TQtIconBrowserItem *clickedItem = (TQtIconBrowserItem*)item;
  TObject *obj = clickedItem->Object();
  if (obj) {
     if (obj->IsFolder()) {
        Clear(obj);
#ifndef OLD
        if (QString(obj->GetName()) == ".." ) {
           emit ActivateParent(obj); 
        } else {
           emit ActivateObject(obj);
        }
#endif
     }
    emit SwitchTreeView(1); // change parent object in TreeView
//    Emit("BrowseObj(TObject*)", (Long_t)obj);
    obj->Browse(Browser());
    emit SwitchTreeView();  // restore the normal flow in TreeView
    FolderExpanded(obj);
  }
}
#ifdef  ATLASTABLEBROWSER
//______________________________________________________________________________
static QString AsString(void *buf, TTable::EColumnType type)
{
  //
  // AsString represents the value provided via "void *b" with type defined
  //          by "name"
  //
  //   void *buf  - the pointer to the value to be printed out.
  //        type  - the basic data type for the value above
  //
   QString out;
   switch (type) {
    case TTable::kFloat:
         out.setNum(*(float *)buf);
         break;
    case TTable::kInt:
         out.setNum(*(int *)buf);
         break;
    case TTable::kLong:
         out.setNum(*(long *)buf);
         break;
    case TTable::kShort:
         out.setNum(*(short *)buf);
         break;
    case TTable::kDouble:
         out.setNum(*(double *)buf);
         break;
    case TTable::kUInt:
         out.setNum(*(unsigned int *)buf);
         break;
    case TTable::kULong:
         out.setNum(*(unsigned long *)buf);
         break;
    case TTable::kUShort:
         out.setNum(*(unsigned short *)buf);
         break;
    case TTable::kUChar:
         out.setNum(*(unsigned char *)buf);
         break;
    case TTable::kChar:
         out.setNum(*(char *)buf);
         break;
    default:
         out = "\"NaN\"";
         break;
   };
   return out;
}
//______________________________________________________________________________
// Check details:
#if QT_VERSION < 0x40000
static void ViewTable(QTable *details, TObject *obj) {
#else /* QT_VERSION */
static void ViewTable(Q3Table *details, TObject *obj) {
#endif /* QT_VERSION */
  TColumnView *view = (TColumnView *)obj;
  TTable *table = (TTable *)view->Table();
  // create a detail view for TTable
   UInt_t arrayLayout[10];
   UInt_t arraySize[10];
    const unsigned char *pointer=0,*startRow=0;
    // define the number of the rows
    Int_t nRows = table->GetNRows();
    Int_t nCols = table->GetNumberOfColumns();
#if QT_VERSION < 0x40000
    QHeader *header = 0;
#else /* QT_VERSION */
    Q3Header *header = 0;
#endif /* QT_VERSION */
    Bool_t vertical = FALSE;
    if (nRows > 1) {
       header = details->horizontalHeader();
       details-> setNumRows (nRows );
       details-> setNumCols (nCols ); 
    } else {
       vertical = true;
       header = details->verticalHeader();
       details-> setNumRows (nCols );
       details-> setNumCols (nRows +1 ); // add one extra column for the "comment" field
    }
    // Name the columns
    int i = 0;
    int k = 0;
    unsigned int kk = 0;
    int j = 0;
    unsigned int jj = 0;
    int colCnt = 0;
    for (i=0;i<nCols;i++) {   
       QString label(table->GetColumnName(i));
       header->setLabel(colCnt,label);
       if (vertical) 
          details->setRowReadOnly(colCnt,true);
       else
          details->setColumnReadOnly(colCnt,true);
       // add the comment for the vertical layout
       if (vertical) {
          QString comment = table->GetColumnComment(i);
          if (!comment.isEmpty()) {
             details->setText(colCnt,nRows,comment);
             details->horizontalHeader()->setLabel(nRows,"Description");
          }
       }
       UInt_t dimensions = table->GetDimensions(i);
       Int_t size = 1;
       if (dimensions) {
          memset(arrayLayout,0,dimensions*sizeof(Int_t));
          const UInt_t *indx = table->GetIndexArray(i);
          // Calculate the array size
          for (jj=0;jj<dimensions;jj++) size *= indx[jj];
          // Check whether array is a char array
          if (table->GetColumnType(i) == TTable::kChar) size = 1;
          // add extra columns for the array elements
          if (size >1) {
             if (vertical) {
                details->insertRows(colCnt,size-1);
                for (j=1;j<size;j++) details->setRowReadOnly(colCnt,true);
             } else {
                details->insertColumns(colCnt,size-1);
                for (j=1;j<size;j++) details->setColumnReadOnly(colCnt+j,true);
             }

             //add extra labels:
             QString numIndx;
             for (j=0;j<size;j++) {
                QString brackets;
                for (kk=0;kk<dimensions;kk++) {brackets += '[' + numIndx.setNum(arrayLayout[kk]) + ']';}
                header->setLabel(colCnt+j,label+brackets);
                label = "";
                ArrayLayout(arrayLayout,arraySize,dimensions);
             }
          }
       }
       colCnt += size;
    }
    if (vertical) {
       details->adjustColumn(nRows);
    }
    // Fill data 
    header = vertical ? details->horizontalHeader() : details->verticalHeader();
    for (j=0;j<nRows;j++) {
       QString num;
       header->setLabel(j,num.setNum(j));
       startRow = (const unsigned char *)table->At(j);
       colCnt = 0;
       for (i=0;i<nCols;i++) {   
          Int_t offset = table->GetOffset(i);
          pointer = startRow+offset;
          UInt_t dimensions = table->GetDimensions(i);
          Int_t size = 1;
          if (dimensions) {
             const UInt_t *indx = table->GetIndexArray(i);
             // Calculate the array size
             for (kk=0;kk<dimensions;kk++) size *= indx[kk];
          }
          TTable::EColumnType type = table->GetColumnType(i);
          if (type == TTable::kChar) {
             QString label;
             for (k=0;k<size&&pointer;k++,pointer++) label += *pointer;
             if (vertical) 
                details->setText(colCnt++,j,label);
             else 
                details->setText(j,colCnt++,label);
          } else {
             UInt_t tSize = table->GetTypeSize(i);
             for (k=0;k<size;k++,pointer += tSize) {
                if (vertical) 
                   details->setText(colCnt++,j,AsString((void *)pointer,type));
                else
                   details->setText(j,colCnt++,AsString((void *)pointer,type));
             }
          }
       }
    }
}
#endif
//______________________________________________________________________________
void TQtIconBrowserImp::Chdir(const TQtBrowserItem *item)
{
   // Make object associated with item the current directory.

   if (item) {
      const TQtBrowserItem *i = item;
      TString dir;
//      while (i) {
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
//         i = i->GetParent(); // the icon view item has no parentbut the IconView
//      }

      if (gDirectory && dir.Length()) gDirectory->cd(dir.Data());
   }
}
//______________________________________________________________________________
void TQtIconBrowserImp::EnableUpdates(Bool_t updt)
{
   // Enable / disanle the widget update
#if QT_VERSION < 0x40000
   QIconView *iconView = (QIconView *)fBrowserImpID->widget(fIconWidgetId);
#else /* QT_VERSION */
   Q3IconView *iconView = (Q3IconView *)fBrowserImpID->widget(fIconWidgetId);
#endif /* QT_VERSION */
   if (updt) {
     // Block this widget update
     fUpdate.UnFreezeToUpdate(iconView);
   } else {
       // Block this widget update
     fUpdate.FreezeToUpdate(iconView);
   }
}
//______________________________________________________________________________
void TQtIconBrowserImp::CreateDetailView()
{
   // Create the separate widget to display the detail view
   if (fDetailWidgetID == -1) {
      // Check whether any useful detail view is available
#if QT_VERSION < 0x40000
     QTable *details = new QTable(fBrowserImpID,"RootDetailView");
#else /* QT_VERSION */
     Q3Table *details = new Q3Table(fBrowserImpID,"RootDetailView");
#endif /* QT_VERSION */
     fDetailWidgetID = fBrowserImpID->addWidget(details);

#if QT_VERSION < 0x40000
     QHeader *header = details->horizontalHeader();
#else /* QT_VERSION */
     Q3Header *header = details->horizontalHeader();
#endif /* QT_VERSION */
     connect(header,SIGNAL(clicked(int)),this,SLOT(SetSortIndicator(int)));
     // details->setSorting (true);
     details-> setNumRows (1); details-> setNumCols (1);
     details->setText ( 0,0,QString("Dummy"));

     header->setLabel(0,QString("No detail view yet!"));
     header = details->verticalHeader();
     header->setLabel(0,QString("0"));
     // detect the table object:
#if QT_VERSION < 0x40000
     QIconView *iconView = (QIconView *)fBrowserImpID->widget(fIconWidgetId);
#else /* QT_VERSION */
     Q3IconView *iconView = (Q3IconView *)fBrowserImpID->widget(fIconWidgetId);
#endif /* QT_VERSION */
     TQtIconBrowserItem *item =(TQtIconBrowserItem *)iconView-> firstItem();
     TObject *rootObject = item ? item->Object():0;
     // We can not use:
     //        if (rootObject->IsA() == TTable::Class() )
     // here to avoid the crash when libTable is not loaded     
     TClass *columnViewClass = gROOT->GetClass("TColumnView");
     if (rootObject && (rootObject->IsA() == columnViewClass) ) {
#ifdef ATLASTABLEBROWSER
        ViewTable(details,rootObject);
#endif
     } else {
     }
//     if (fCurrentItem) 
//     {
//        TObject *rootObject = fCurrentItem->Object();
//        if (rootObject->IsA() == TTable::Class() ) {
//           ViewTable(details,(TTable *)rootObject);
//        }
//     } 
   }
}
//______________________________________________________________________________
void TQtIconBrowserImp::FolderExpanded(TObject * /*obj*/,Bool_t expand) 
{
    fFolderExpanded = expand;
}
//______________________________________________________________________________
void TQtIconBrowserImp::CreateIcons()
{
  if ( !folderClosed ) {
#if QT_VERSION < 0x40000
    folderClosed = new QIconSet(QPixmap( folder_closed_xpm ));
    fileNormal   = new QIconSet(QPixmap( pix_file ));
#else /* QT_VERSION */
    folderClosed = new QIcon(QPixmap( folder_closed_xpm ));
    fileNormal   = new QIcon(QPixmap( pix_file ));
#endif /* QT_VERSION */
  }
}
//______________________________________________________________________________ 
QPaintDevice *TQtIconBrowserImp::GetBrowserID() { return fBrowserImpID; }
//______________________________________________________________________________ 
#if QT_VERSION < 0x40000
void TQtIconBrowserImp::PopMenu(QIconViewItem *item, const QPoint &pos)
#else /* QT_VERSION */
void TQtIconBrowserImp::PopMenu(Q3IconViewItem *item, const QPoint &pos)
#endif /* QT_VERSION */
{
  TQtIconBrowserItem *that = (TQtIconBrowserItem*)item;
  TContextMenu *menu = Browser()->GetContextMenu();
  if (that) { 
    if (that->Object() && menu)
               menu->Popup(pos.x(),pos.y(), that->Object(),Browser());
  } else {
   // if (menu) menu->Popup(pos.x(),pos.y(), Browser(),Browser());
  }
}
//______________________________________________________________________________
#if QT_VERSION < 0x40000
void TQtIconBrowserImp::SelectionChanged(QIconViewItem * /*item*/) 
#else /* QT_VERSION */
void TQtIconBrowserImp::SelectionChanged(Q3IconViewItem * /*item*/) 
#endif /* QT_VERSION */
{  
  //fprintf(stderr," Selection changed\n"); 
}
//______________________________________________________________________________
void TQtIconBrowserImp::SetViewMode(int mode){
   // public slot SetViewMode(int mode)
   // turn the different view of the widget

   //  QIconView::Arrangement:
   //  ----------------------
   //   QIconView::LeftToRight - Items which don't fit into the view go further down (you get a vertical scrollbar) 
   //   QIconView::TopToBottom - Items which don't fit into the view go further right (you get a horizontal scrollbar) 

   //   QIconView::ItemTextPos
   //   ----------------------
   //   QIconView::Bottom - The text is drawn below the icon. 
   //   QIconView::Right - The text is drawn to the right of the icon. 

   //   QIconSet::Size
   //   --------------
   //   QIconSet::Small - The pixmap is the smaller of two. 
   //   QIconSet::Large - The pixmap is the larger of two. 

   // MS Icons - LeftToRight + Bottom (middle size icons)
   //    tiles      - TopToBottom + Right (large icon)
   //    list       - tiles + very Small icon
   //    details    - table view
  if ( int(fCurentViewMode) == mode) return;
  fCurentViewMode = TQtGui::TQtIconViewOptions(mode);
   if (mode == TQtGui::kViewDetails) {
      CreateDetailView();
      if (fDetailWidgetID != -1) 
         fBrowserImpID->raiseWidget ( fDetailWidgetID );
   } else {
#if QT_VERSION < 0x40000
      QIconView::Arrangement arrangement   = QIconView::LeftToRight;
      QIconView::ItemTextPos labelPosition = QIconView::Bottom;
      QIconSet::Size   size = QIconSet::Large;
#else /* QT_VERSION */
      Q3IconView::Arrangement arrangement   = Q3IconView::LeftToRight;
      Q3IconView::ItemTextPos labelPosition = Q3IconView::Bottom;
      QIcon::Size   size = QIcon::Large;
#endif /* QT_VERSION */
      int xGridSize = 68; // for the large icon
      int yGridSize = 36; // for the large icon
      switch (mode) {
         case TQtGui::kViewSmallIcons:
#if QT_VERSION < 0x40000
            arrangement   = QIconView::TopToBottom;
            labelPosition = QIconView::Right;
#else /* QT_VERSION */
            arrangement   = Q3IconView::TopToBottom;
            labelPosition = Q3IconView::Right;
#endif /* QT_VERSION */
            xGridSize *= 2;
            yGridSize  = 32;
            break;
         case TQtGui::kViewList:
#if QT_VERSION < 0x40000
            arrangement   = QIconView::TopToBottom;
            labelPosition = QIconView::Right;
            size          = QIconSet::Small;
#else /* QT_VERSION */
            arrangement   = Q3IconView::TopToBottom;
            labelPosition = Q3IconView::Right;
            size          = QIcon::Small;
#endif /* QT_VERSION */
            xGridSize  = int(2.5*xGridSize);
            yGridSize  = 16;
            break;
         case TQtGui::kViewDetails:
         default:
#if QT_VERSION < 0x40000
            arrangement   = QIconView::LeftToRight;
            labelPosition = QIconView::Bottom;
            size = QIconSet::Large;
#else /* QT_VERSION */
            arrangement   = Q3IconView::LeftToRight;
            labelPosition = Q3IconView::Bottom;
            size = QIcon::Large;
#endif /* QT_VERSION */
            // xGridSize = 68;
            // yGridSize = 36;
            break;
      };
      // Block this widget update
#if QT_VERSION < 0x40000
      QIconView *iconView = (QIconView *)fBrowserImpID->widget(fIconWidgetId);
#else /* QT_VERSION */
      Q3IconView *iconView = (Q3IconView *)fBrowserImpID->widget(fIconWidgetId);
#endif /* QT_VERSION */
      EnableUpdates (kFALSE);
      {
         iconView->setArrangement (arrangement );
         iconView->setItemTextPos (labelPosition);
         SetIconSize(size);
         iconView->setGridX(xGridSize);
         iconView->setGridY(yGridSize);
#if QT_VERSION < 0x40000
         iconView->setResizeMode(QIconView::Adjust);
#else /* QT_VERSION */
         iconView->setResizeMode(Q3IconView::Adjust);
#endif /* QT_VERSION */
         // set  (reset) new pixmap size for all items;
#if QT_VERSION < 0x40000
         QIconViewItem *item;
#else /* QT_VERSION */
         Q3IconViewItem *item;
#endif /* QT_VERSION */
         for ( item = iconView->firstItem(); item; item = item->nextItem() )
            ((TQtIconBrowserItem*)item)->SetPixmap(fIconSize);
      } 
      EnableUpdates (kTRUE);
      fBrowserImpID->raiseWidget (fIconWidgetId);
   }
}
//______________________________________________________________________________
void TQtIconBrowserImp::BrowseObj(TObject *obj) 
{ 
  if (obj) {
    Add(obj,0);
    fRootObject = obj;
#ifndef OLD
    TBrowser *b = 0;
    b = Browser();
    if (b) obj->Browse(b); 
#else
    if (fBrowserCustom)  obj->Browse(fBrowserCustom);
#endif
  }
}
//______________________________________________________________________________
void TQtIconBrowserImp::ExecuteDefaultAction(TObject *obj) {
   TQtRootBrowserAction::Instance()->ExecuteDefaultAction(obj);
}
//______________________________________________________________________________
void TQtIconBrowserImp::Iconify() { if (fBrowserImpID) fBrowserImpID->hide(); }
//______________________________________________________________________________
void TQtIconBrowserImp::RecursiveRemove(TObject * /*obj*/) { }
//______________________________________________________________________________
void TQtIconBrowserImp::Refresh(Bool_t /*flag*/) { }
//______________________________________________________________________________
void TQtIconBrowserImp::Show() { if (fBrowserImpID)  fBrowserImpID->show(); }

//______________________________________________________________________________
Int_t TQtIconBrowserImp::InitWindow(Bool_t show)
{
   // Create a fake custom browser
   fBrowserCustom  = (TBrowserCustom *)TBrowserCustom::Class()->New();
   if (fBrowserCustom) fBrowserCustom->SetBrowserImp(this);

#if QT_VERSION < 0x40000
   fBrowserImpID = new QWidgetStack(0,"RootIconBrowser",Qt::WDestructiveClose);
#else /* QT_VERSION */
   fBrowserImpID = new Q3WidgetStack(0,"RootIconBrowser",Qt::WDestructiveClose);
#endif /* QT_VERSION */
//   fBrowserImpID = new QIconView(0,"RootBrowser",Qt::WDestructiveClose);
   fBrowserImpID->setCaption(fTitle);
   if (fX*fY) fBrowserImpID->setGeometry(fX,fY,fWidth,fHeight);
   else fBrowserImpID->resize(fWidth,fHeight);

   // create the icon view 
#if QT_VERSION < 0x40000
   QIconView *iconView = new QIconView(fBrowserImpID,"RootIconView");
#else /* QT_VERSION */
   Q3IconView *iconView = new Q3IconView(fBrowserImpID,"RootIconView");
#endif /* QT_VERSION */
   fIconWidgetId = fBrowserImpID->addWidget(iconView);
#if QT_VERSION < 0x40000
   iconView->setResizeMode(QIconView::Adjust);
#else /* QT_VERSION */
   iconView->setResizeMode(Q3IconView::Adjust);
#endif /* QT_VERSION */
   iconView->setWordWrapIconText(FALSE);
   SetViewMode(TQtGui::kViewSmallIcons);

#if QT_VERSION < 0x40000
   connect(iconView, SIGNAL(clicked ( QIconViewItem *))
      ,this, SLOT(ClickedItem(QIconViewItem*)));
#else /* QT_VERSION */
   connect(iconView, SIGNAL(clicked ( Q3IconViewItem *))
      ,this, SLOT(ClickedItem(Q3IconViewItem*)));
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(iconView, SIGNAL(selectionChanged ( QIconViewItem * ))
      , this, SLOT(SelectionChanged(QIconViewItem *))); 
#else /* QT_VERSION */
   connect(iconView, SIGNAL(selectionChanged ( Q3IconViewItem * ))
      , this, SLOT(SelectionChanged(Q3IconViewItem *))); 
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(iconView, SIGNAL(rightButtonPressed( QIconViewItem *, const QPoint &))
      ,this, SLOT(PopMenu(QIconViewItem *, const QPoint &))); 
#else /* QT_VERSION */
   connect(iconView, SIGNAL(rightButtonPressed( Q3IconViewItem *, const QPoint &))
      ,this, SLOT(PopMenu(Q3IconViewItem *, const QPoint &))); 
#endif /* QT_VERSION */

#if QT_VERSION < 0x40000
   connect(iconView, SIGNAL(contextMenuRequested( QIconViewItem *, const QPoint &))
      ,this, SLOT(PopMenu(QIconViewItem *, const QPoint &))); 
#else /* QT_VERSION */
   connect(iconView, SIGNAL(contextMenuRequested( Q3IconViewItem *, const QPoint &))
      ,this, SLOT(PopMenu(Q3IconViewItem *, const QPoint &))); 
#endif /* QT_VERSION */

   iconView->setShowToolTips(true);
   if (show) Show();
   return 0;
}

//______________________________________________________________________________
TQMimeTypes *TQtIconBrowserImp::IconList() 
{
   if (gfMimeTypeList) return gfMimeTypeList;
   // Load GUI defaults from .rootrc
  char icon_path[1024];
  char line[1024];
  char mime_file[1024];
#ifndef R__VMS
# ifdef ROOTICONPATH
   sprintf(icon_path, "%s/icons:%s:.:",
           gSystem->Getenv("HOME"),
           ROOTICONPATH);
#  ifdef EXTRAICONPATH
   strcat(icon_path, gEnv->GetValue("Gui.IconPath", EXTRAICONPATH));
#  else
   strcat(icon_path, gEnv->GetValue("Gui.IconPath", ""));
#  endif
# else
   sprintf(icon_path, "%s/icons:%s/icons:.:", gSystem->Getenv("HOME"),
                                              gSystem->Getenv("ROOTSYS"));
   strcat(icon_path, gEnv->GetValue("Gui.IconPath", ""));
# endif
   sprintf(line, "%s/.root.mimes", gSystem->Getenv("HOME"));
#else
   sprintf(line,"[%s.ICONS]",gSystem->Getenv("ROOTSYS"));
   strcpy(icon_path, gEnv->GetValue("Gui.IconPath",line));
   sprintf(line,"%sroot.mimes",gSystem->Getenv("HOME"));
#endif

   strcpy(mime_file, gEnv->GetValue("Gui.MimeTypeFile", line));
   char *mf = gSystem->ExpandPathName(mime_file);
   if (mf) {
      strcpy(mime_file, mf);
      delete [] mf;
   }
   if (gSystem->AccessPathName(mime_file, kReadPermission))
#ifdef R__VMS
      sprintf(mime_file,"[%s.ETC]root.mimes",gSystem->Getenv("ROOTSYS"));
#else
# ifdef ROOTETCDIR
      sprintf(mime_file, "%s/root.mimes", ROOTETCDIR);
# else
      sprintf(mime_file, "%s/etc/root.mimes", gSystem->Getenv("ROOTSYS"));
# endif
#endif
   gfMimeTypeList = new TQMimeTypes(icon_path, mime_file);
   return  gfMimeTypeList;
}
//__slot: ________________________________________________________________________
void TQtIconBrowserImp::SetSortIndicator(int section) 
{ 
   // Set the columns sort indicator
#if QT_VERSION < 0x40000
   QHeader *header = (QHeader *)sender ();
#else /* QT_VERSION */
   Q3Header *header = (Q3Header *)sender ();
#endif /* QT_VERSION */
#if (QT_VERSION < 0x030200)
   if (section&&header) {}
#else
  // this works with Qt 3.2 and later #if (QT_VERSION >= 0x040000)
#if QT_VERSION < 0x40000
   SortOrder currentOrder = Qt::Ascending;
#else /* QT_VERSION */
   Qt::SortOrder currentOrder = Qt::AscendingOrder;
#endif /* QT_VERSION */
   if ( ( header->sortIndicatorSection () == section  ) && 
#if QT_VERSION < 0x40000
        ( header->sortIndicatorOrder   () == Qt::Ascending) ) {
#else /* QT_VERSION */
        ( header->sortIndicatorOrder   () == Qt::AscendingOrder) ) {
#endif /* QT_VERSION */
       // change the sort order
#if QT_VERSION < 0x40000
       currentOrder = Qt::Descending;
#else /* QT_VERSION */
       currentOrder = Qt::DescendingOrder;
#endif /* QT_VERSION */
   }
   header->setSortIndicator ( section, currentOrder );
#if QT_VERSION < 0x40000
   QTable *details = (QTable*)fBrowserImpID->widget(fDetailWidgetID);
   details->sortColumn(section,currentOrder==Qt::Ascending,true);
#else /* QT_VERSION */
   Q3Table *details = (Q3Table*)fBrowserImpID->widget(fDetailWidgetID);
   details->sortColumn(section,currentOrder==Qt::AscendingOrder,true);
#endif /* QT_VERSION */
#endif
}
