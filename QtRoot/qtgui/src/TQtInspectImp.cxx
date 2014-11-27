// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtInspectImp.cxx,v 1.11 2013/08/30 16:00:24 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TQtInspectImp.h"
#include "TGQt.h"
#include "Buttons.h"
#include "TClass.h"
#include "TRealData.h"
#include "TDataType.h"
#include "TDataMember.h"
#include "TIterator.h"
#include "TList.h"
#include "TLink.h"
#include "TDatime.h"

#include "qclipboard.h"
#include "qapplication.h"

#include <QTableWidgetItem>
#include <QHeaderView>
#include <QtDebug>
#include <QTableWidget>

class TQtInspectorItem : public   QTableWidgetItem 
                            {
private:
  TLink *fLink;
public:
  TQtInspectorItem(QTableWidget*parent, QString labelName, QString label2Value,QString label3Title, TLink *link)
    :  QTableWidgetItem(labelName),fLink(link) 
  {
      Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;
      int rows = parent->rowCount();
      parent->setRowCount (rows+1);      
      parent->setItem(rows,0,this); 
      setText(labelName);
      setFlags(flags);
       
      QTableWidgetItem *item= new QTableWidgetItem();       
      item->setText(label2Value);
      item->setFlags(flags);
      parent->setItem(rows,1,item);
      if (label2Value.contains("->")) {
         // mark clickable linl with the blue foreground
          this->setForeground(Qt::blue);
          item->setForeground(Qt::blue);
      }
      item= new QTableWidgetItem();
      item->setText(label3Title);
      item->setFlags(flags);
      parent->setItem(rows,2,item);
//       qDebug() << "Row" << rows+1<<":"<< label << label2 << label3;

  }
   TLink *Link(){ return fLink;} 
  ~TQtInspectorItem(){delete Link(); }
};

// ClassImp(TQtInspectWidget)

///////////////////////////////////////////////////////////////
//                                                           //
//   TQtInspectWidget is a special Qt object to implement    //
//   TObject::Inspect  member funection                      //
//                                                           //
///////////////////////////////////////////////////////////////

//______________________________________________________________________________
TQtInspectWidget::TQtInspectWidget(QWidget *parent,const TObject *obj) :
                                  QTableWidget(parent)
, fInspector(0)
 {
   CreateInspector(obj);
   connect(this,SIGNAL(itemClicked(QTableWidgetItem *)),SLOT(Selected(QTableWidgetItem *)));
   setAttribute(Qt::WA_DeleteOnClose);
}

//______________________________________________________________________________
TQtInspectWidget::TQtInspectWidget(TQtInspectImp  *parent,const TObject *obj) :
                                  QTableWidget()
, fInspector(parent)
 {
   CreateInspector(obj);
   connect(this,SIGNAL(itemClicked(QTableWidgetItem *)),SLOT(Selected(QTableWidgetItem *)));
   setAttribute(Qt::WA_DeleteOnClose);
}

//______________________________________________________________________________
void TQtInspectWidget::CreateInspector(const TObject *obj)
{
  fObject = obj;
  setSortingEnabled(false);
  setColumnCount(3);
  MakeTitle();
  MakeHeaders();
  AddValues();

  // Enable the sorting
  verticalHeader()->hide();
  setSortingEnabled(true);

}

//______________________________________________________________________________
void TQtInspectWidget::MakeHeaders()
{

  const char *headers[3];
  headers[0] = "Member Name";
  headers[1] = "Value";
  headers[2] = "Title";

  Int_t widths[]  = {96,120, 320};
  int i;
  int lHeader = sizeof(headers)/sizeof(const char *);
  for (i=0;i<lHeader;i++){
    QTableWidgetItem *item = new QTableWidgetItem();
    item->setText(tr(headers[i]));
    setHorizontalHeaderItem(i, item);
    setColumnWidth(i, widths[i]);
  }  
}
//______________________________________________________________________________
void TQtInspectWidget::MakeTitle()
{
    TClass *cl = fObject->IsA();
    if (cl == 0) return;

    char buffer[1024];
    sprintf(buffer, "%s   :   %s:%d   -   \"%s\"  -> 0x%p", cl->GetName(),
                                           fObject->GetName(),
                                           fObject->GetUniqueID(),
                                           fObject->GetTitle(),fObject);
    setCaption(buffer);
}
//______________________________________________________________________________
void TQtInspectWidget::AddValues()
{
    Int_t cdate = 0;
    Int_t ctime = 0;
    UInt_t *cdatime = 0;
    Bool_t isdate = kFALSE;
    enum {kname, kvalue, ktitle};

    QString line[ktitle+1];

    TClass *cl = fObject->IsA();
    if (cl == 0) return;
    if (!cl->GetListOfRealData()) cl->BuildRealData();

//*-*- count number of data members in order to resize the canvas
    TRealData *rd;
    TIter      next(cl->GetListOfRealData());
    Int_t nreal = cl->GetListOfRealData()->GetSize();
    if (nreal == 0)  return;

//*-*  Prepare a list view control for adding a large number of items
//*-*
    while ( ( rd = (TRealData*) next() ) ) {
       TDataMember *member = rd->GetDataMember();
       TDataType *membertype = member->GetDataType();
       isdate = kFALSE;
       if (strcmp(member->GetName(),"fDatime") == 0 && strcmp(member->GetTypeName(),"UInt_t") == 0)
       {
          isdate = kTRUE;
       }
//*-*- Encode data member name
       line[kname] = (char *)(rd->GetName());

//*-*- Encode data value or pointer value
       Int_t offset = rd->GetThisOffset();
       char *pointer = (char*)fObject + offset;
       char **ppointer = (char**)(pointer);
       TLink *tlink = 0;

       if (member->IsaPointer()) {
          char **p3pointer = (char**)(*ppointer);
          if (!p3pointer) {
             line[kvalue] = "->0";
          } else if (!member->IsBasic()) {
             line[kvalue] = QString("->0x%1").arg((ulong)p3pointer,2*sizeof(ulong),16,QChar('0'));
             tlink = new TLink(0, 0, p3pointer);
          } else if (membertype){
               if (!strcmp(membertype->GetTypeName(), "char"))
                  line[kvalue] = *ppointer;
               else
                  line[kvalue] =  membertype->AsString(p3pointer);
          }
          else if (!strcmp(member->GetFullTypeName(), "char*") ||
                   !strcmp(member->GetFullTypeName(), "const char*")) {
             line[kvalue] =  *ppointer;
          } else {
             line[kvalue] = QString("->0x%1").arg( (ulong)p3pointer,2*sizeof(ulong),16,QChar('0'));
             tlink = new TLink(0, 0, p3pointer);
          }
       } else if (membertype)
            if (isdate) {
               cdatime = (UInt_t*)pointer;
               TDatime::GetDateTime(cdatime[0],cdate,ctime); 
               line[kvalue] = QString("%1/%2").arg(cdate).arg(ctime);
            } else {
               line[kvalue] =  membertype->AsString(pointer);
            }
       else
           line[kvalue] = QString("->0x%1").arg((ulong)pointer,2*sizeof(ulong),16,QChar('0'));

    //*-*- Encode data member title

       if (strcmp(member->GetFullTypeName(), "char*") &&
           strcmp(member->GetFullTypeName(), "const char*")) {
           line[ktitle] = (char *)member->GetTitle();
       }
       if (tlink) {
         tlink->SetName((char *)member->GetTypeName());
         tlink->SetBit(kCanDelete);
       }
       
       new TQtInspectorItem(this,line[kname],line[kvalue],line[ktitle],tlink);
//       if (tlink) { Add(tlink); ctrl->Add(tlink); } 
    }
}
//______________________________________________________________________________
TQtInspectWidget::~TQtInspectWidget()
{ 
   hide(); 
   delete Disconnect();
}
//______________________________________________________________________________
void TQtInspectWidget::Hide(){ hide(); }
//______________________________________________________________________________
void TQtInspectWidget::Show(){ 
   resizeColumnToContents(2); 
 //  show();
   raise();
   showNormal();
//   gVirtualX->RaiseWindow(TGQt::iwid(this)); 
}

//______________________________________________________________________________
void TQtInspectWidget::Selected(QTableWidgetItem *myItem)
{
  // - Expand the selected item (if possible)
  // - Copy the selected item to the system clipboard

  if (myItem) {
    QString clipboardText;

    int row = myItem->row();
    clipboardText += this->item(row,0)->text();
    clipboardText += this->item(row,1)->text();
    clipboardText += this->item(row,2)->text();

    if ( ! clipboardText.isEmpty() ){
      // Copy it to the system clipboard
      QClipboard *cb = QApplication::clipboard();
      cb->setText(clipboardText);
    }
    // Execute ROOT action
    TQtInspectorItem *it = 0;
    it = (TQtInspectorItem *)this->item(row,0);
    if (it) {
       TLink *tlink = it->Link();
       if (tlink) tlink->ExecuteEvent(kButton1Up,0,0);
    }
  }
}
//______________________________________________________________________________
TQtInspectImp *TQtInspectWidget::Disconnect()
{
   TQtInspectImp *save = fInspector;
   fInspector = 0;
   if (save) save->Disconnect();
   return save;
}

//______________________________________________________________________________
//
//              TQtInspectImp
//______________________________________________________________________________
TQtInspectImp::TQtInspectImp(const TObject *obj, UInt_t width, UInt_t height)
{
    fWidget = new  TQtInspectWidget(this, obj);  
    fWidget->resize(width,height);
    Show();
}
//______________________________________________________________________________
TQtInspectImp::~TQtInspectImp()
{
   // Destroy the widget
   Hide();
   delete Disconnect();
}
//______________________________________________________________________________
void  TQtInspectImp::Hide()
{
   // Hide the widget
   if (fWidget) fWidget->Hide();
}
//______________________________________________________________________________
void  TQtInspectImp::Show()
{
   // Show the widget
   if (fWidget) fWidget->Show();
}
//______________________________________________________________________________
TQtInspectWidget *TQtInspectImp::Disconnect()
{
   // To be called by TQtInspectWidget dtor
   TQtInspectWidget *save = fWidget;
   fWidget = 0;
   if (save) save->Disconnect();
   return save;
}
