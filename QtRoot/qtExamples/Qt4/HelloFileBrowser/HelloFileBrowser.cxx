//Added by qt3to4:
#include "HelloFileBrowser.h"
#include <QWhatsThis>
#include <QLineEdit>
#include <QCursor>
#include <string>
#include "TH1.h"
#include "TH2.h"
#include "TROOT.h"
#include "TApplication.h"

#include "TFile.h"
#include "TKey.h"
#include "TList.h"

/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/
//__________________________________________________________________
HelloFileBrowser::HelloFileBrowser(QWidget* parent)
: QWidget(parent)
{
   setupUi(this);
   init();
}
//__________________________________________________________________
HelloFileBrowser::~HelloFileBrowser(){;}
//__________________________________________________________________
void HelloFileBrowser::init()
{
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
// Make sure the ROOT graphical layer is initialised.
static struct needgraph {   needgraph () {  TApplication::NeedGraphicsLibs() ;  gApplication->InitializeGraphics();} }  needgraph;
#endif

   // Attach the validator to facilitate the ROOT <tab> completion
   // fTabCompValidator = new TQtTabValidator(comboBox1);
   // comboBox1->setValidator(fTabCompValidator);

   splitter_2->setStretchFactor(0,1);
   splitter_2->setStretchFactor(1,50);

   // Predefine to ROOT command
   comboBox->addItem("gROOT->Macro(\"qcanvas.CC(gPad)\");");

   // Make the the embedded TCanvas to be the current TPad
   // In other words :-)
   // assign  the "gPad" pointer pointing to the TCanvas object embeded into the QWidget one

   currentWidget = widget; currentWidget->cd();

   // Connect the QLineEditor with the ROOT command interpreter
   comboBox->setLineEdit(new QLineEdit ( comboBox) ); 
   connect(comboBox->lineEdit(),SIGNAL(returnPressed ()),this, SLOT( execRoot()) );
   
   connect(treeView, SIGNAL(Activated(TObject * )),
           this,     SLOT(TreeView_Clicked(TObject * )));
   
   connect(widget,SIGNAL(RootEventProcessed(TObject *, unsigned int, TCanvas *))
         ,this,   SLOT(CanvasEvent(TObject *, unsigned int, TCanvas *)));
   
   connect(widget_2,SIGNAL(RootEventProcessed(TObject *, unsigned int, TCanvas *))
         ,this,     SLOT(CanvasEvent(TObject *, unsigned int, TCanvas *)));
   widget->EnableSignalEvents(kMousePressEvent);
   widget_2->EnableSignalEvents(kMouseMoveEvent);
   // Create a separate wigdet with ROOT Object browser, Just in case
   // new TBrowser();
}
//__________________________________________________________________
void HelloFileBrowser::destroy()
{}
//__________________________________________________________________
void HelloFileBrowser::TreeView_Clicked(TObject *o)

{
   if(o && !o->InheritsFrom(TFile::Class())){ // Do not touch to TFile object
      
      currentWidget = widget;
      if(dynamic_cast<TH2*> (o) ) {
         currentWidget = widget_2;
      } else if( dynamic_cast<TH1*> (o) ) {
         currentWidget = widget;
      }
      if (currentWidget) {
//         TObject *Th;
         // std::string objectKey = SelectedItem->text().toStdString();
         // Th = fxDiskFile->Get(objectKey.c_str());
         currentWidget->Clear();
         currentWidget->Draw(o);
         currentWidget->Refresh();
      }
   }
}

//__________________________________________________________________
void HelloFileBrowser::execRoot()
{
  // TQtTabValidator::Clear(); 
  std::string cintCommand = comboBox->lineEdit()->text().toStdString();
  gROOT->ProcessLine(cintCommand.c_str());
  gROOT->ProcessLine("gPad->Update();");
}


//__________________________________________________________________
void HelloFileBrowser::widget_destroyed( QObject * )
{

}

//__________________________________________________________________
void HelloFileBrowser::CanvasEvent(TObject *obj, unsigned int /*event*/, TCanvas *)
{
  TQtWidget *tipped = (TQtWidget *)sender();
  const char *objectInfo = 
        obj->GetObjectInfo(tipped->GetEventX(),tipped->GetEventY());
  QString tipText ="<P>You have ";
  if  (tipped == widget)
     tipText +="clicked";
  else
     tipText +="passed";
  tipText += " the object:<b> ";
  tipText += obj->GetName();
  tipText += "</b> of class <b>"; 
  tipText += obj->ClassName();
  tipText += "</b> : ";
  tipText += objectInfo;
  

  if  (tipped == widget) {
     static bool ax = false;     
     if (obj->IsA() != TAxis::Class() || !ax) {
        ax = true;
        QWhatsThis::showText( QCursor::pos (),tipText); // , globalPosition,tipped);
     }
  } else {
     tipped->setToolTip(tipText);
  }
}
//__________________________________________________________________
void HelloFileBrowser::SetRootFile(const QString &file)
{
   if (treeView) treeView->SetRootFile(file);
}
//__________________________________________________________________
void HelloFileBrowser::SetLastWorkingDir(const QString &dir)
{
      if (treeView) treeView->SetLastWorkingDir(dir);
}
//__________________________________________________________________
void HelloFileBrowser::languageChange(){}
