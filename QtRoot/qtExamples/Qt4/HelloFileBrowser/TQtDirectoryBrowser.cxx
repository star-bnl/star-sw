#include "TQtObjectViewFrame.h"
#include <QStandardItem>
#include <QString>
#include <QFileIconProvider>
#include "TList.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TRealData.h"
#include "TDirectory.h"
#include "TKey.h"
#include "TH3.h"
#include "TH2.h"
#include <QDebug>

//_________________________________________________________________
static QStandardItem *CreateItem(TDirectory  *parentDir) 
{
   // Create tree view of the ROOT TDirectory
   TDirectory *saveDir = (parentDir == gDirectory) ? 0 : gDirectory;
   if (saveDir) parentDir->cd();

   QStandardItem *item = 0;
   item =  TQtObjectViewFrame::ClearEditFlag(new QStandardItem(parentDir->GetName()));
   item->setData(qVariantFromValue((void *)parentDir));
   item->setIcon(TQtObjectViewFrame::Icon(QFileIconProvider::Folder)); 

   TList *listOfKeys = parentDir->GetListOfKeys();
   TIter next(listOfKeys);
   TObject *key = 0;
   while ((key = next())) { 
      TObject *obj = ((TKey *)key)->ReadObj();
      QStandardItem *nextItem = 0;
      TDirectory    *nextDir  = 0;
      if ( (nextDir = dynamic_cast<TDirectory*>(obj) ) ) {
         nextItem  =  CreateItem(nextDir);
      } else {
         nextItem =  TQtObjectViewFrame::ClearEditFlag(new QStandardItem(obj->GetName()));
      }
      nextItem->setData(qVariantFromValue((void *)obj));

      QList<QStandardItem *>  columns;
      columns << nextItem
              <<  TQtObjectViewFrame::ClearEditFlag(new QStandardItem(obj->ClassName()))
              <<  TQtObjectViewFrame::ClearEditFlag(new QStandardItem(obj->GetTitle()))
          ;
      item->appendRow(columns);
      if (!nextDir) {
         if (obj->InheritsFrom(TH3::Class())) 
            nextItem->setIcon(QIcon("h3_s.xpm"));
         else if (obj->InheritsFrom(TH2::Class()))
            nextItem->setIcon(QIcon("h2_s.xpm"));
         else  if (obj->InheritsFrom(TH1::Class()))
            nextItem->setIcon(QIcon("h1_s.xpm"));
         else  if (obj->InheritsFrom("TNtuple"))
            nextItem->setIcon(QIcon("ntuple_s.xpm"));
         else  if (obj->InheritsFrom("TTree"))
            nextItem->setIcon(QIcon("tree_s.xpm"));
         else 
            nextItem->setIcon(TQtObjectViewFrame::Icon(QFileIconProvider::File));
     }
   }
   if (saveDir) saveDir->cd();
   return item;
}

//_________________________________________________________________
QStandardItem *TQtObjectViewFrame::CreateItem(TDirectory *dir, QStandardItem *parentItem)
{
   // Create the top view 
   if (dir) {
      QStandardItem *item = ::CreateItem(dir);
       QList<QStandardItem *>  columns;
       columns  << item
                << TQtObjectViewFrame::ClearEditFlag(new QStandardItem(dir->ClassName()))
                << TQtObjectViewFrame::ClearEditFlag(new QStandardItem(dir->GetTitle()))
           ;

      parentItem->appendRow(columns);
   }
   return parentItem;
}
