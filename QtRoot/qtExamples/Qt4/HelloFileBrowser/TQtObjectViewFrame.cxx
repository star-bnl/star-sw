#include "TQtObjectViewFrame.h"

#include <QTreeView>
#include <QHeaderView>
#include <QStandardItemModel>
#include <QStandardItem>
#include <QString>
#include <QFileDialog>
#include <QDirModel>
#include <QDir>
#include <QDebug>

#include "TQtInspectImp.h"
#include "TFile.h"
#include "TROOT.h"

class QWidgetLockUpdate {
private:
   QWidget &fW;
   bool    fWasLocked;
public:
   QWidgetLockUpdate(QWidget &w) :fW(w),fWasLocked(!w.updatesEnabled())
   {      
      if (!fWasLocked) {
         w.setCursor(Qt::WaitCursor);
         w.setUpdatesEnabled(false);
      }
   }
   ~QWidgetLockUpdate() 
   {
       if (!fWasLocked) {
          fW.unsetCursor();     
          fW.setUpdatesEnabled(true);
          fW.update();
       }
   }

};
//______________________________________________________________
QStandardItem *TQtObjectViewFrame::ClearEditFlag(QStandardItem *item)
{
   if (item) {
      item->setFlags(item->flags() & ( ~Qt::ItemIsEditable)); 
   }
   return item;
}
//______________________________________________________________
TQtObjectViewFrame::TQtObjectViewFrame(QWidget * parent)
      : QTreeView(parent), fModel(0),fROOTFileNameItem(0)
       ,fDaqBufferItem(0),fDirectory(0),fLastWorkingDir(".")
{
   fModel = new QStandardItemModel(0,3,this);
   this->header()->setDefaultAlignment(Qt::AlignHCenter);
#if 0
   this->setColumnWidth(0,200);  // Object name
   this->setColumnWidth(1,64);   // Object class
   this->setColumnWidth(2,200);  // Object title
#endif

   ResetView();
   this->setModel(fModel);
   connect(this, SIGNAL(clicked(QModelIndex)),
         this, SLOT(Clicked(const QModelIndex&))); 
   connect(this, SIGNAL(expanded(QModelIndex)),
         this, SLOT(Expand(const QModelIndex&)));
   connect(this, SIGNAL(activated(QModelIndex)),
          this, SLOT(ActivateObject(const QModelIndex&))); 

}
//______________________________________________________________
TQtObjectViewFrame::~TQtObjectViewFrame()
{ 
   // dtor
   delete fDirectory; fDirectory = 0;
}
//______________________________________________________________
void TQtObjectViewFrame::ResetView() 
{ 
   
   QString saveFileName; 
   if (fROOTFileNameItem) {      
      saveFileName = fROOTFileNameItem->text();
      fModel->clear();
//      delete fROOTFileNameItem;
      fROOTFileNameItem = 0;
   }
   QStringList labels;
   labels << "Object" << "Class" << "Title";
   fModel->setHorizontalHeaderLabels (labels);
   fROOTFileNameItem = ClearEditFlag(new QStandardItem());
   
   fROOTFileNameItem->setIcon(Icon(QFileIconProvider::Drive));
   if (!saveFileName.isEmpty()) {
       fROOTFileNameItem ->setText(saveFileName);
    }
   fROOTFileNameItem->setToolTip("Click the \"Hard Drive\" icon  to open the new ROOT file");
   QStandardItem *parentItem = fModel->invisibleRootItem();
   parentItem->appendRow(fROOTFileNameItem);
}

//______________________________________________________________
void TQtObjectViewFrame::SetRootFile(const QString &fileName)
{ 
   QWidgetLockUpdate(*this);
   QFileInfo fi(fileName);
   fROOTFileNameItem->setText(fi.fileName());
   fFullFileName = fileName;
   ResetView();
   // this->resizeColumnToContents(0); 
   SetLastWorkingDir(fileName);
   OpenRootFile();
}
//______________________________________________________________
void TQtObjectViewFrame::SetDirectoryBuffer(TDirectory *buffer, const QString &/*header*/)
{
   if (buffer != fDirectory) 
   { 
      QWidgetLockUpdate(*this);
      ResetView();
      fDirectory = buffer;
      fDaqBufferItem = CreateItem(fDirectory,fROOTFileNameItem);
      this->expandToDepth(4);    
      // this->resizeColumnToContents(0);  
   } else {
      printf(" No buffer\n");
   }
}
//______________________________________________________________
void TQtObjectViewFrame::OpenRootFile()
{
   if (fDirectory) { delete fDirectory; fDirectory = 0;}
   TFile *file = TFile::Open(fFullFileName.toStdString().c_str());
   SetDirectoryBuffer(file," HEADER" );
}
//______________________________________________________________
void TQtObjectViewFrame::ActivateObject(const QModelIndex &index)
{
  QStandardItem *item = fModel->itemFromIndex(index);
  if (item) 
  {
     if (item->column()) {
        item = item->parent()->child(item->row());
     }
     QVariant cont = item->data();
     void *obj = cont.value<void *>();
     TObject *rootObject = (TObject *)obj;
     if (rootObject) emit Activated(rootObject);
  }
}
//______________________________________________________________
void TQtObjectViewFrame::Clicked(const QModelIndex &index)
{
   QStandardItem *item = fModel->itemFromIndex(index);
   // Do stuff with the item ...
   if (item) 
   {
 //     TString className = item->text().toStdString().c_str();
      if (item == fROOTFileNameItem) {
         // manage the file name
         if (fROOTFileNameItem->hasChildren () || fROOTFileNameItem->text().isEmpty()) {            
            // open the new file 
            QString rootFile;
            { QWidgetLockUpdate(*this);
                rootFile = QFileDialog::getOpenFileName(this,tr("Open ROOT file"),
                fLastWorkingDir,
                tr("ROOT files (*.root);; All files (*)") );
            }
            if (!rootFile.isEmpty()) {
               SetRootFile(rootFile);
               emit newRootFile(rootFile);
            }
         }
      }
#if 0 
      else  if (gROOT->GetClass(className.Data()))  {
         QVariant cont = item->data();
         void *obj = cont.value<void *>();
         TQtInspectWidget *inspector= new TQtInspectWidget(0,className.Data(),(const void*)obj);
         inspector->Show();
         // Toggle "collapse / expanded
         if (this->isExpanded (index)) {
            this->collapse(index);
         } else {
            this->expand(index);
         }
      }
#else
      else {
         ActivateObject(index);
      }
#endif
   }
}

//______________________________________________________________
void TQtObjectViewFrame::Expand(const QModelIndex &index)
{
   // Mimic the filename click when file item is to be expanded
   if (fModel->itemFromIndex(index) == fROOTFileNameItem) {
      if (fLastExpandedFile != fROOTFileNameItem->text()) {
         fLastExpandedFile = fROOTFileNameItem->text();  
         Clicked(index);
      }
   }
} 

//______________________________________________________________
void TQtObjectViewFrame::SetLastWorkingDir(const QString &fileName) 
{ 
   fLastWorkingDir = QDir().filePath(fileName);
}

//______________________________________________________________
QFileIconProvider *TQtObjectViewFrame::fgIcons=0;

//______________________________________________________________
const QIcon &TQtObjectViewFrame::Icon(QFileIconProvider::IconType it) 
{
   static QIcon ic;
   if (!fgIcons) {
      QDirModel *m = new QDirModel();
      fgIcons = m->iconProvider(); //new QFileIconProvider(); 
   }
   return ic=fgIcons->icon(it);
}
