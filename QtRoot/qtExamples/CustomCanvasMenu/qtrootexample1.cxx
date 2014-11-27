//Added by qt3to4:
#include <q3mimefactory.h>
#include <QMenu>
#include <QLineEdit>
#include "qtrootexample1.h"
#include "TKey.h"
#include "TFile.h"
#include "TList.h"
#include "TROOT.h"
#include "TClass.h"
#include "TQtCustomizeCanvasMenu.h"

/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/
//__________________________________________________________________
qtrootexample1::qtrootexample1(QWidget *parent): QWidget(parent)
{ 
   setupUi(this);
   init();
}
//__________________________________________________________________
void qtrootexample1::init()
{
   fxDiskFile = 0;
   // Connect the QLineEditor with the ROOT command interpreter
   connect(comboBox1->lineEdit(),SIGNAL(returnPressed ()), SLOT( execRoot()) );
   
   // Attach the validator to facilitate the ROOT <tab> completion
   // fTabCompValidator = new TQtTabValidator(comboBox1);
   // comboBox1->setValidator(fTabCompValidator);

   // Predefine to ROOT command
   comboBox1->insertItem("gROOT->Macro(\"qcanvas.CC(gPad)\");");
   comboBox1->insertItem(".x hsimple.C");
   comboBox1->insertItem(".q");
   
   // Make the the embedded TCanvas to be the current TPad
   // In other words :-)
   // assign  the "gPad" pointer pointing to the embeded into the QWidget a TCanvas 
   
   currentWidget = tQtWidget1; currentWidget->cd();
   
   // Open pre-defined ROOT file
   TKey *key = 0;
   fxDiskFile=new TFile("test.root");
   TIter next(fxDiskFile->GetListOfKeys());
   // Fill the "tree view" with the object info from the file
   while((key = (TKey*) next())) {
      AddItemToListView1(key->ReadObj());
   }
   connect(tQtWidget1,SIGNAL(RootEventProcessed(TObject *, unsigned int, TCanvas *)),this,SLOT(CanvasEvent(TObject *, unsigned int, TCanvas *)));
   connect(tQtWidget2,SIGNAL(RootEventProcessed(TObject *, unsigned int, TCanvas *)),this,SLOT(CanvasEvent(TObject *, unsigned int, TCanvas *)));
   tQtWidget1->EnableSignalEvents(kMousePressEvent);
   tQtWidget2->EnableSignalEvents(kMouseMoveEvent);
   
   // Add the custom Context menu
   TQtCustomizeCanvasMenu *contextFilter = TQtCustomizeCanvasMenu::installCustomMenu(tQtWidget1);
   connect(contextFilter,SIGNAL(AboutToShow(QMenu *,TContextMenu *))
             , this,SLOT(CustomizeIt(QMenu *,TContextMenu *)));
   contextFilter = TQtCustomizeCanvasMenu::installCustomMenu(tQtWidget2);

   connect(contextFilter,SIGNAL(AboutToShow(QMenu *,TContextMenu *))
             , this,SLOT(CustomizeIt(QMenu *,TContextMenu *)));
  // Create a separate wigdet with ROOT Object browser, Just in case
   // new TBrowser();
}

//__________________________________________________________________
void qtrootexample1::ListView1_mouseButtonPressed( int, Q3ListViewItem *SelectedItem, const QPoint &, int )
{
   if(SelectedItem!=0){
      currentWidget = tQtWidget1;
      if("TH2" == (SelectedItem->text(1))) {
         currentWidget = tQtWidget2;
      } else if("TH1" == (SelectedItem->text(1))) {
         currentWidget = tQtWidget1;
      }
      if (currentWidget && fxDiskFile ) {
         TObject *Th;
         currentWidget->cd();
         Th = fxDiskFile->Get(SelectedItem->text(0));
         Th->Draw();
         currentWidget->Refresh();
      }
   }
}
//__________________________________________________________________
void qtrootexample1::AddItemToListView1(TObject *key)
{
   if( key->IsA()->InheritsFrom("TH2") ) {
      Q3ListViewItem * item1 = new Q3ListViewItem(ListView1,  key->GetName() ,"TH2");
      item1->setPixmap(0 , qPixmapFromMimeSource( "h2_t.png"));
   }else if (key->IsA()->InheritsFrom("TH1")) {
      Q3ListViewItem * item1 = new Q3ListViewItem(ListView1,   key->GetName(),"TH1");
      item1->setPixmap( 0, qPixmapFromMimeSource( "h1_t.png"));
   }
}
//__________________________________________________________________
void qtrootexample1::execRoot()
{
  // TQtTabValidator::Clear(); 
  gROOT->ProcessLine(comboBox1->lineEdit()->text());
  gROOT->ProcessLine("gPad->Update();");
}


//__________________________________________________________________
void qtrootexample1::CanvasEvent(TObject *obj, unsigned int /*event*/, TCanvas *)
{
  TQtWidget *tipped = (TQtWidget *)sender();
  const char *objectInfo = 
        obj->GetObjectInfo(tipped->GetEventX(),tipped->GetEventY());
  QString tipText ="You have ";
  if  (tipped == tQtWidget1)
     tipText +="clicked";
  else
     tipText +="passed";
  tipText += " the object <";
  tipText += obj->GetName();
  tipText += "> of class "; 
  tipText += obj->ClassName();
  tipText += " : ";
  tipText += objectInfo;
  tipped->setToolTip(tipText);
}
//_______________________________________________________
void qtrootexample1::CustomizeIt (QMenu *contextMenu, TContextMenu *rootContextMenu) const
{ 
  // Add the custom item to the ROOT Context Menu.
  // One can use the second parameters to gather n extra information if needed
  if ( rootContextMenu ) {
     contextMenu->addSeparator();
     contextMenu->addMenu("&AtlasDAQ")
     ->addMenu("Idea to customize ROOT menu belongs Andrea Dotti");
   }
}
