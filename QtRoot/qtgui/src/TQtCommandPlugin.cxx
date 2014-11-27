// @(#)root/gui:$Id: TQtCommandPlugin.cxx,v 1.2 2013/08/30 16:00:23 perev Exp $
// Author: Bertrand Bellenot   26/09/2007
#include "TQtCommandPlugin.h"
#include "ui_TQtRootCommand.h"

#include <QLineEdit>
#include <QPalette> 

#include <QFile>
#include <QDir>
#include <QTextStream>
#include <QString>
#include <QTimer>

#include "TQtWidget.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TRint.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "Getline.h"

//_____________________________________________________________________________
//
// TQtCommandPlugin
//
// Class used to redirect command line input/output.
//_____________________________________________________________________________

// ClassImp(TQtCommandPlugin)

//______________________________________________________________________________
TQtCommandPlugin::TQtCommandPlugin(QWidget * parent, Qt::WindowFlags f) :
      QFrame(parent,f),fUi(new Ui_TQtRootCommand())
     ,fTempFile( QString("%1/command.%2.log").arg(QDir::tempPath()).arg(QCoreApplication::applicationPid ()),this )
     ,fTimer(this)
{
   // TQtCommandPlugin Constructor.
   fUi->setupUi(this);
   InsertFromHistory();
   
   connect(fUi->fComboCmd->lineEdit(),SIGNAL(returnPressed ()), this, SLOT( HandleCommand()) );
   connect(fUi->fComboCmd->lineEdit(),SIGNAL(returnPressed ()), fUi->fComboCmd->lineEdit(), SLOT( clear()) );

   fTimer.setInterval(1000);
   connect(&fTimer,SIGNAL(timeout ()), this, SLOT(CheckRemote()));
}

//_____________________________________________________________________________
void TQtCommandPlugin::InsertFromHistory(int index) 
{
   QString defhist =  QDir::homePath () + "/.root_hist";
   QFile lunin(defhist);
   if (lunin.open(QIODevice::ReadOnly | QIODevice::Text)) {
     QTextStream in(&lunin);
     while (!in.atEnd()) fUi->fComboCmd->insertItem(index,in.readLine());
   }
}

//______________________________________________________________________________
TQtCommandPlugin::~TQtCommandPlugin()
{
   // Destructor.
   fTempFile.remove();
   delete fUi; fUi = 0;
}

//______________________________________________________________________________
void TQtCommandPlugin::CheckRemote()
{
   // Check if actual ROOT session is a remote one or a local one.
   TApplication *app = gROOT->GetApplication();
   if (app->InheritsFrom("TRint"))  {
      TString sPrompt = ((TRint*)app)->GetPrompt();
      Int_t end = sPrompt.Index(":root [", 0);
      QPalette textColor =  fUi->fLabel->palette ();
      fUi->fLabel->setForegroundRole(QPalette::WindowText);
      if (end > 0 && end != kNPOS) {
         // remote session
         sPrompt.Remove(end);
         textColor.setColor(QPalette::WindowText,QColor(Qt::red));
         fUi->fLabel->setPalette(textColor);
         fUi->fLabel->setText(QString("Command (%1):").arg(sPrompt.Data()));
      } else {
         // local session
         textColor.setColor(QPalette::WindowText,QColor(Qt::black));
         fUi->fLabel->setText("Command (local):");
      }
   }
}

//______________________________________________________________________________
void TQtCommandPlugin::HandleCommand()
{
   // Handle command line from the "command" combo box.

   QString string = fUi->fComboCmd->lineEdit()->text(); 
   if (!string.isEmpty() ) {
      if (!gApplication) TQtWidget::InitRint();     
      QString sPrompt = "root []";
      TApplication *app = gROOT->GetApplication();
      if (app->InheritsFrom("TRint"))
         sPrompt = ((TRint*)gROOT->GetApplication())->GetPrompt();
      if (fTempFile.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
         fTempFile.write(QString("%1%2\n").arg(sPrompt).arg(string).toLatin1());
         fTempFile.close();
      }
      QApplication::setOverrideCursor (Qt::BusyCursor);
      gSystem->RedirectOutput(fTempFile.fileName().toLatin1().data(), "a");
      gApplication->SetBit(TApplication::kProcessRemotely);
      gROOT->ProcessLine(string.toLatin1().data());
      if (app->InheritsFrom("TRint"))
         Gl_histadd((char *)string.toLatin1().data());
      gSystem->RedirectOutput(0);      
      if (fTempFile.open(QIODevice::ReadOnly)) {
            fUi->fStatus->appendPlainText(fTempFile.readAll());
            fTempFile.close();
      }
      QApplication::restoreOverrideCursor();
      CheckRemote();
   }
}
