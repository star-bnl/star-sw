#include "ui_HelloSignal.h"
#include "HelloSignal.h"
#include "TQtRootSlot.h" 
#include "TROOT.h"
#include <string>
#include <QLineEdit>

//_________________________________________________________
HelloSignal::HelloSignal(QWidget *parent) 
: fGuiWidget(0), fLineEdit(0) 
{
     fGuiWidget = new QWidget(parent);
     fUi = new Ui::HelloSignal;
     Ui::HelloSignal &ui = *fUi;
     ui.setupUi(fGuiWidget);
     fLabel  = ui.label;
     fLineEdit =  ui.comboBox->lineEdit();
     connect(fLineEdit,SIGNAL(editingFinished()),this,SLOT(NewRootCommand()));
     connect(ui.toolButton,SIGNAL(clicked() ),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit())); 
     connect(ui.comboBox,SIGNAL(currentIndexChanged(int)), this, SLOT(SetPrompt(int)));
     Show();
}

//_________________________________________________________
HelloSignal::~HelloSignal()
{
    delete fUi;
}
//_________________________________________________________
void HelloSignal::Show() {
   if (fGuiWidget) fGuiWidget->show();
}
//_________________________________________________________
void HelloSignal::Hide() {
   if (fGuiWidget) fGuiWidget->hide();
} 
//_________________________________________________________
void HelloSignal::NewRootCommand() {
  if (fLineEdit) {
     std::string text = fLineEdit->text().toStdString();
     fLastString = text.c_str() ;
     gROOT->ProcessLine(fLastString.Data());
     fLineEdit->clear();
  }
}
//_________________________________________________________
const TString &HelloSignal::Text() const {
   return fLastString;
}
//_________________________________________________________
void HelloSignal::SetPrompt(int indx)
{
   QString a = QString("root[%1]").arg(indx);
   if ( fLabel)  fLabel->setText(a);
}
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
#include "TApplication.h"
// Make sure the ROOT graphical layer is initialised.
static struct needgraph {   needgraph () {  TApplication::NeedGraphicsLibs() ;  gApplication->InitializeGraphics();} }  needgraph;
#endif
HelloSignal *gHelloSignal = new HelloSignal();
