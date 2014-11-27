//Added by qt3to4:
#include "qtrootexample1.h"
#include <QWhatsThis>
#include <QLineEdit>
#include <QCursor>
#include <string>
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
qtrootexample1::qtrootexample1(QWidget* parent, const char* name, Qt::WindowFlags fl)
: QWidget(parent)
{
   setupUi(this);
   init();
}
//__________________________________________________________________
qtrootexample1::~qtrootexample1(){;}
//__________________________________________________________________
void qtrootexample1::init()
{
   fxDiskFile = 0;
   
   // Attach the validator to facilitate the ROOT <tab> completion
   // fTabCompValidator = new TQtTabValidator(comboBox1);
   // comboBox1->setValidator(fTabCompValidator);

   // Predefine to ROOT command
   comboBox->addItem("gROOT->Macro(\"qcanvas.CC(gPad)\");");
   comboBox->addItem(".x hsimple.C");
   comboBox->addItem(".q");
   
   // Make the the embedded TCanvas to be the current TPad
   // In other words :-)
   // assign  the "gPad" pointer pointing to the embeded into the QWidget a TCanvas 
   
   currentWidget = widget; currentWidget->cd();
   
   // Open pre-defined ROOT file
   TKey *key = 0;
   fxDiskFile=new TFile("test.root");
   TIter next(fxDiskFile->GetListOfKeys());
   // Fill the "tree view" with the object info from the file
   while((key = (TKey*) next())) {
      AddItemToListView(key->ReadObj());
   }
   // Connect the QLineEditor with the ROOT command interpreter
   comboBox->setLineEdit(new QLineEdit ( comboBox) ); 
   connect(comboBox->lineEdit(),SIGNAL(returnPressed ()),this, SLOT( execRoot()) );
   connect(listWidget,SIGNAL(itemClicked(QListWidgetItem * )),
           this,SLOT(ListView_Clicked(QListWidgetItem * )));
   connect(widget,SIGNAL(RootEventProcessed(TObject *, unsigned int, TCanvas *))
         ,this,SLOT(CanvasEvent(TObject *, unsigned int, TCanvas *)));
   connect(widget_2,SIGNAL(RootEventProcessed(TObject *, unsigned int, TCanvas *))
         ,this,SLOT(CanvasEvent(TObject *, unsigned int, TCanvas *)));
   widget->EnableSignalEvents(kMousePressEvent);
   widget_2->EnableSignalEvents(kMouseMoveEvent);
   // Create a separate wigdet with ROOT Object browser, Just in case
   // new TBrowser();
}
//__________________________________________________________________
void qtrootexample1::destroy()
{}
//__________________________________________________________________
void qtrootexample1::ListView_Clicked(QListWidgetItem *SelectedItem )

{
   if(SelectedItem!=0){
      currentWidget = widget;
      if("TH2" == (SelectedItem-> whatsThis () )) {
         currentWidget = widget_2;
      } else if("TH1" == (SelectedItem->whatsThis ())) {
         currentWidget = widget;
      }
      if (currentWidget && fxDiskFile ) {
         TObject *Th;
         currentWidget->cd();
         std::string objectKey = SelectedItem->text().toStdString();
         Th = fxDiskFile->Get(objectKey.c_str());
         Th->Draw();
         currentWidget->Refresh();
      }
   }
}
//__________________________________________________________________
void qtrootexample1::AddItemToListView(TObject *Key)
{
   if (Key) {
      QListWidgetItem * item1 = 0;
      if( Key->IsA()->InheritsFrom("TH2") ) {
         item1 = new QListWidgetItem(QIcon("images/h2_t.png"), 
                Key->GetName(), listWidget);
         item1->setWhatsThis("TH2");
      }else if (Key->IsA()->InheritsFrom("TH1")) {
         item1 = new QListWidgetItem(QIcon("images/h1_t.png"), 
                Key->GetName(), listWidget);
         item1->setWhatsThis("TH1");
      }
      listWidget->addItem(item1);
   }
}
//__________________________________________________________________
void qtrootexample1::execRoot()
{
  // TQtTabValidator::Clear(); 
  std::string cintCommand = comboBox->lineEdit()->text().toStdString();
  gROOT->ProcessLine(cintCommand.c_str());
  gROOT->ProcessLine("gPad->Update();");
}


//__________________________________________________________________
void qtrootexample1::widget_destroyed( QObject * )
{

}

//__________________________________________________________________
void qtrootexample1::CanvasEvent(TObject *obj, unsigned int /*event*/, TCanvas *)
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
      QWhatsThis::showText( QCursor::pos (),tipText); // , globalPosition,tipped);
  } else {
     tipped->setToolTip(tipText);
  }
}
//__________________________________________________________________
void qtrootexample1::languageChange(){}
