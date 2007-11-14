#if R__QT
//*-- Author :    Valery Fine   14/08/01  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: StEventControlPanel.cxx,v 1.19 2007/11/14 22:42:16 fine Exp $
//

////////////////////////////////////////////////////////////////////////
//
// This macro generates a Controlbar panel: 
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/EventControlPanel.gif" ></P> end_html
//
// To execute an item, click with the left mouse button.
//  
// Just start this macro wheneven you want:
//
//  From Root/Cint macro:
//  --------------------
//   .x EventControlPanel.C
//   .L EventControlPanel.C
// or
//   gROOT->LoadMacro("EventControlPanel.C");
//
//  From the compiled C++ code:
//  --------------------
//   gROOT->LoadMacro("EventControlPanel.C");
//
//  After that one may "click" <4 views> button to get from the single "view"
//  the expanded view as follows:
//  begin_html  <P ALIGN=CENTER> <IMG SRC="gif/FourStarView.gif" ></P> end_html
//  To cutomize the default bar the dirived class with the custom void UserBar()
//  method can be done.
//         TControlBar *Bar(){ return fBar;}  
//  method can be used.
//
//  Example:
//    TControlBar *myBar =   __aa__.Bar();
//     myBar->AddButton("My custom","printf(\"here is my custom action\n\");","To add your own action replace the second parameter"); 
//
//  Note:  If you don't like what it does make your private copy 
//  ====   change it with your favorite text editor and load it right
//         away.
//         NO EXTRA STEP like : compilation, linking, loading required 
//
///////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////
//
//  PadControl panel is the set of the static methods to control 
//  TView of any "current" TPad with some "primitive"
//  operation:
//
///////////////////////////////////////////////////////////////////////

#include "StEventControlPanel.h"
#include "StEventDisplayInfo.h"
#include "StSimplePanel.h"
#include "StPadControlPanel.h"
#include "StFilterDialog.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TList.h"
#include "TBrowser.h"
#include "TVolume.h"

#include "TQtRootBrowserImp.h"

#include "StChain.h"
#include "StMaker.h"
#include "StEventDisplayMaker.h"
#include "StEventHelper.h"
#include "TClass.h"
// #include "StFilterABC.h"

#include <qapplication.h>
#include <qbuttongroup.h>
#include <qcheckbox.h>
#include <qvbuttongroup.h>
#include <qhbuttongroup.h> 
#include <qpushbutton.h>
#include <qobjectlist.h>
#include <qvbox.h>
#include <qhbox.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qtabwidget.h> 
#include <qmessagebox.h>

StChain             *StEventControlPanel::fgChain  = 0;
StEventDisplayInfo  *StEventControlPanel::fgHlp    = 0;
StEventDisplayMaker *StEventControlPanel::fgDispMk = 0;

//_______________________________________________________________________________________
StEventControlPanel::StEventControlPanel() : fBar(0), fFilter(0),fDialog(0),fBrowser(0)
{ 
   TClass *kl = gROOT->GetClass("StChain");
   if (kl && kl->GetClassInfo())
   {
      fgChain  = (StChain *)StMaker::GetChain();
      fgDispMk = (StEventDisplayMaker*)fgChain->GetMaker("EventDisplay");
   }
   Build();
}
//_______________________________________________________________________________________
StEventControlPanel::~StEventControlPanel()
{   delete fBar; }
//_______________________________________________________________________________________
void StEventControlPanel::AddButt(const Char_t *buttonName, const Char_t *command)
{   
   (new QPushButton(buttonName,fBar,command))->setToggleButton(true);
}
//_______________________________________________________________________________________
void StEventControlPanel::Build()
{
   const char *listEvents[] = {
         "point"	    ,"dst/point(id_track,position[0]:position[1]:charge)",
         "primtrk"    ,"dst/primtrk"			         ,
         "vertex"	    ,"dst/vertex(vtx_id,x:y:z)"	,
         "globtrk"    ,"dst/globtrk"			         ,
         
         " "  	 ,"StEvent(Tpc Used Hits)"	   ,
         "TPC"	 ,"StEvent(Tpc Unused Hits)"	, 
         " "  	 ,"StEvent(Ftpc Used Hits)"	   ,
         "FTPC" ,"StEvent(Ftpc Unused Hits)"	, 
         " "	 ,"StEvent(Svt Used Hits)"	   , 
         "SVT"	 ,"StEvent(Svt Unused Hits)"	, 
         " "	 ,"StEvent(Ssd Used Hits)"	   , 
         "SSD"	 ,"StEvent(Ssd Unused Hits)"	, 
         " " 	 ,"StEvent(RnD Used Hits)"	   ,
         "RnD"  ,"StEvent(RnD Unused Hits)"	, 
         " "	 ,"StEvent(Emc Used Hits)"	   , 
         "EMC"	 ,"StEvent(Emc Unused Hits)"	, 
         " "	 ,"StEvent(Tof Used Hits)"    	, 
         "TOF"	 ,"StEvent(Tof Unused Hits)"	, 
         "All Used Hits"  		,"StEvent(All Used Hits)"  	,
         "All Unused Hits"		,"StEvent(All Unused Hits)"	, 

         " " 		,"StEvent(Global Tracks)"	   ,
         "Global"	,"StEvent(Global Track Hits)", 
         " " 		,"StEvent(Primary Tracks)"	   ,
         "Primary"	,"StEvent(Primary Track Hits)", 
         " "    		,"StEvent(Kink Tracks)"		   ,
         "Kink"  	,"StEvent(Kink Track Hits)"   , 
         " "      		,"StEvent(V0 Tracks)"		   ,
         "V0"		,"StEvent(V0 Track Hits)"	   ,  
         " "      		,"StEvent(Xi Tracks)"	   	,
         "Xi"		,"StEvent(Xi Track Hits)"	   , 
         "All Tracks"    		,"StEvent(All Tracks)"	   	,
         "All Track Hits"		,"StEvent(All Track Hits)"	   ,

         0};      
      int i = 0;
      int n = 0;
      fBar = new QVBox(0,"Event Control Panel");
      fBar->setCaption("Event Control Panel");
      QHBox *fControlBox = new QHBox(fBar,"Event Control Panel");
      QGroupBox *eventGroup = new QGroupBox("Event Control Panel",fControlBox);
      eventGroup->setColumnLayout(1,Qt::Vertical);
      QVBox *fPadControl = new QVBox(eventGroup,"Event Control Panel");
      new StPadControlPanel(fControlBox);



      //----------------------------
      // Table data
      int nButtons = 4;
      StSimplePanel *panel = new StSimplePanel(fPadControl,"dstPanel",2);
      panel->setTitle("dst");
      for (n=0;n<nButtons;n++) {
        QCheckBox *box = new QCheckBox(listEvents[i],panel,listEvents[i+1]);
        panel->Add(box,n>>1,n&1,Qt::AlignLeft);
        connect(box,SIGNAL(clicked()),this,SLOT(Clicked()));
        i += 2;
      }
      //----------------------------
      // StEvent hits
      //----------------------------
      nButtons = 16;
      panel = new StSimplePanel(fPadControl,"hitsPanel",4);
      panel->setTitle("Hits (StEvent) Detectors");

      QPushButton *all[3];
      all[0] = new QPushButton("",panel,"");
      all[0]->setText("All");
      all[1] = new QPushButton("",panel,"");
      all[1]->setText("All");
      
      all[2] = new QPushButton("",panel,"");
      all[2]->setText("All");

      QLabel *label = new QLabel("Used",panel);
      panel->Add(label,0,0);

      label = new QLabel("Unused",panel);
      panel->Add(label,0,1);
      QHButtonGroup *geomSrc = new QHButtonGroup("geom",panel);
      panel->AddMulti(geomSrc,2,-1,0); geomSrc->setFlat(TRUE);
      geomSrc->setExclusive(true);
      QPushButton *geom = new QPushButton("G3",geomSrc);
                   geom->setToggleButton(true);
                   geom = new QPushButton("STI",geomSrc);
                   geom->setToggleButton(true);
      connect(geomSrc,SIGNAL(clicked(int)),this,SLOT(Clicked(int)));

      const char *detectorNames[] =
      {
         "TPC"	 ,"TPSS"	, 
         "FTPC" ,"SCON"	, 
         "SVT"	 ,"STSI"	, 
         "SSD"	 ,"SFSM"	, 
         "RnD"  ,  0	   , 
         "EMC"	 ,"CALB"	, 
         "TOF"	 ,"BTOF"	
      };

      int nDetector = 0;
      for (n=2;n<nButtons+2;n++) {
         if (strstr(listEvents[i],"All")) {
            all[n&1]->setName(listEvents[i]);
         }else {
            QCheckBox *box = new QCheckBox(panel,listEvents[i+1]);
            panel->Add(box,n>>1,n&1);
            connect(all[n&1],SIGNAL(clicked()),box,SLOT(animateClick()));
            connect(box,SIGNAL(clicked()),this,SLOT(Clicked()));
            if (n&1) {
               QLabel *label =  new QLabel(listEvents[i],panel);
               panel->Add(label,n>>1,2,Qt::AlignLeft);
               // Add volume control buttons
               const char *dName = detectorNames[(2*nDetector++)+1];
               if (dName) {
                  box = new QCheckBox(panel,dName);
                  connect(box,SIGNAL(clicked()),this,SLOT(ClickedVolume()));
                  connect(all[2],SIGNAL(clicked()),box,SLOT(animateClick()));
                  panel->Add(box,n>>1,3);
               }
            }
         }
         i += 2;
      }
      // Add all butoons
      panel->Add(all[0],n,0); panel->Add(all[1],n,1); panel->Add(all[2],n,3);
#ifndef STAR_BUTTON
#include "starIconTrans.xpm"
      QPushButton *detectorBrowser = new QPushButton(panel,"STAR");
      detectorBrowser->setPixmap(QPixmap(starIconTrans));
      connect(detectorBrowser,SIGNAL(clicked()),this,SLOT(ShowVolumeBrowser()));
      panel->Add(detectorBrowser,n,2);
#endif
      
      //----------------------------
      // StEvent tracks
      //----------------------------
      nButtons = 10;
      panel = new StSimplePanel(fPadControl,"trackPanel");
      panel->setTitle("Track (StEvent)");

      all[0] = new QPushButton("",panel,"");
      all[0]->setText("All");
      all[1] = new QPushButton("",panel,"");
      all[1]->setText("All");

      label = new QLabel("tracks",panel);
      panel->Add(label,0,0);

      label = new QLabel("hits",panel);
      panel->Add(label,0,1);

      for (n=2;n<nButtons+2;n++) {
         if (strstr(listEvents[i],"All")) {
            all[n&1]->setName(listEvents[i]);
         } else {
            if (listEvents[i+1]) {
               QCheckBox *box = new QCheckBox(panel,listEvents[i+1]);
               panel->Add(box,n>>1,n&1);
               connect(all[n&1],SIGNAL(clicked()),box,SLOT(animateClick()));
               connect(box,SIGNAL(clicked()),this,SLOT(Clicked()));
               if (n&1) {
                  QLabel *label =  new QLabel(listEvents[i],panel);
                  panel->Add(label,n>>1,2,Qt::AlignLeft);
               }
            }
         }
         i += 2;
      }
      // Add all butoons
      panel->Add(all[0],n,0); panel->Add(all[1],n,1);

      //----------------------------
      // Some actions
      //----------------------------
      QWidget *buttons = new QWidget(fBar,"misc");
      {
         QBoxLayout * l = new QHBoxLayout( buttons );
         l->addItem(new QSpacerItem(10,1));

         fRedrawButton = new QPushButton("ReDraw",buttons,"redraw");
         connect(fRedrawButton,SIGNAL(clicked()),this,SLOT(Redraw()));
         l->addWidget( fRedrawButton );

         l->addItem(new QSpacerItem(10,1));

         fNextEventButton = new QPushButton("Next",buttons,"next");
         connect(fNextEventButton,SIGNAL(clicked()),this,SLOT(NextEvent()));
         l->addWidget( fNextEventButton );

         l->addItem(new QSpacerItem(10,1));

         QPushButton *button = new QPushButton("Filter",buttons,"filter");
         // if (!fFilter) button->setDisabled(true);
         connect(button,SIGNAL(clicked()),this,SLOT(ShowFilter()));
         l->addWidget( button );

         l->addItem(new QSpacerItem(10,1));

#ifdef  SELECTION_BUTTON
         button = new QPushButton("Selection",buttons,"print");
         // if (!fFilter) button->setDisabled(true);
         connect(button,SIGNAL(clicked()),this,SLOT(PrntNames()));
         l->addWidget( button );
#endif 
         l->addItem(new QSpacerItem(10,1));
      }
}
//_______________________________________________________________________________________
/* public slot: */ 
void StEventControlPanel::Clicked(int id) 
{ 
    // Select the geometry Geant / Sti
   if (fgDispMk) { 
      fgDispMk->SetGeomType(id);
      fgDispMk->ClearGeometry();
   }
}
//_______________________________________________________________________________________
void StEventControlPanel::ClickedVolume() 
{ 
   if (!fgDispMk) return;
   QButton *b = (QButton *)sender();
   if (b) {
      if (b->isToggleButton ()) {
         switch ( b->state()) {
         case QButton::On: 
            fgDispMk->AddVolume(b->name()); 
            fgDispMk->ClearGeometry();
            break;
         case QButton::Off:
            fgDispMk->RemoveVolume(b->name()); 
            fgDispMk->ClearGeometry();
            break;
         case QButton::NoChange:
            break;
         };
      }
   }
}  
//_______________________________________________________________________________________
void StEventControlPanel::Clicked() 
{ 
   if (!fgDispMk) return;
   QButton *b = (QButton *)sender();
   if (b) {
      if (b->isToggleButton ()) {
         switch ( b->state()) {
         case QButton::On: 
            {
               TList *lis = fgDispMk->GetNameList(); 
               if (!lis || !(lis->FindObject(b->name()))) {
                  fgDispMk->AddName(b->name(),kFALSE);
               }
               break;
            }
         case QButton::Off:
            fgDispMk->RemoveName(b->name());
            break;
         case QButton::NoChange:
            break;
         };
      }
   }
}  
//_______________________________________________________________________________________
void StEventControlPanel::SuspendTopWidget()
{
   fSavedCursor = fBar->cursor ();
   fBar->setCursor(QCursor(Qt::WaitCursor));
   fBar->topLevelWidget ()->setEnabled (FALSE );
   QApplication::beep ();
}
//_______________________________________________________________________________________
void StEventControlPanel::ResumeTopWidget(){
   QApplication::beep ();
   fBar->setCursor(fSavedCursor);
   fBar->topLevelWidget ()->setEnabled (TRUE );
}

//_______________________________________________________________________________________
//
//     Qt  public slots:
//_______________________________________________________________________________________
void StEventControlPanel::NextEvent()
{
   SuspendTopWidget(); 

   fgDispMk->MakeInfo("Looping over the next event");
   {gSystem->DispatchOneEvent(1);}
   fBar->topLevelWidget ()->setEnabled (FALSE );
   fgChain->EventLoop(1,1);   
//   StEventDisplayMaker::MakeLoop(2);
   fgDispMk->MakeInfo("Done");
   ResumeTopWidget();
   {gSystem->DispatchOneEvent(1);}
}
//_______________________________________________________________________________________
void StEventControlPanel::Redraw()
{
   SuspendTopWidget(); 

   fgDispMk->MakeInfo("Redrawing the current event");
   {gSystem->DispatchOneEvent(1);}
   fgDispMk->ClearCanvas();
   fgDispMk->Redraw();
   fgDispMk->MakeInfo("Done");

   ResumeTopWidget();
   {gSystem->DispatchOneEvent(1);}
}
//_______________________________________________________________________________________
void StEventControlPanel::PrntNames()
{ PrintNames() ; }

//_______________________________________________________________________________________
void StEventControlPanel::PrintNames()
{
   if (!fgDispMk) return;
   TList *tl = fgDispMk->GetNameList();
   if (!fgHlp) new StEventDisplayInfo(&fgHlp, "Draw Selections", 200, 100);
   TListIter nextOne(tl);
   TObject *n=0;
   int nk=0;
   while ((n=nextOne())) {
      if (nk) {fgHlp->AddText(n->GetName());}
      else    {fgHlp->SetText(n->GetName());}
      nk=1;
   }
   fgHlp->Popup();
}
//_______________________________________________________________________________________
void StEventControlPanel::Refresh()
{
   if (!fgDispMk) return;
   TList *tl = fgDispMk->GetNameList();
   const TList  *lGeom = fgDispMk->GetVolumeNameList();
   if (!tl) return;
   QCheckBox *but=0;
   TObject *n=0;
   QObjectList *l = fBar->queryList( "QCheckBox" );
   QObjectListIt nextButton( *l ); // iterate over the buttons
   while ( (but = (QCheckBox *)nextButton.current())) {
      //      printf(" StEventControlPanel::Refresh() %s %s\n",(const char *)but->name());
      but->blockSignals(kTRUE);
      ++nextButton;
      TListIter nextParameter(tl);
      while ((n=nextParameter())) {
         if (strchr(n->GetName(),'(')==0)      continue;
         if (strstr(but->name(),n->GetName())) break; 
      }
      if (n ) {
         if (!but->isChecked())  but->setChecked(true);
      } else {
         // Check may be it is the "volume" button
         TListIter nextParameter(lGeom);
         TObject *v  = 0;
         while ((v=nextParameter())) {
            if (strstr(but->name(),v->GetName())) break; 
         }
         if (v ) {
            if (!but->isChecked()) but->setChecked(true);
         } else {
            if (but->isChecked()) but->setChecked(false);
         }
      }
      but->blockSignals(kFALSE);
   }
   delete l;
   gSystem->DispatchOneEvent(1);
}
//_______________________________________________________________________________________
void StEventControlPanel::AddFilter(TObject *filter)
{   
   if (!fFilter) fFilter = new TList();
   fFilter->Add(filter);
}

//_______________________________________________________________________________________
void StEventControlPanel::Show()
{
   // Show group of buttons.  
   Refresh();
   fBar->show();
}

//_______________________________________________________________________________________
void StEventControlPanel::ShowFilter()
{  // fFilter->Show();
   // Create StFilterDialog
   if (!fFilter) {
      QMessageBox::warning ( 0,  "Show Event Filter"
                                  ,"No filter has been defined yet!"
                                  , QMessageBox::Ok, QMessageBox::NoButton);
         return;
   }
   int nFilters = fFilter->GetSize();
   delete fDialog; // We want to keep the only filter on the screen
   QTabWidget *tabView = 0;
   if (nFilters > 1) {
      // Create tab view
      tabView = new QTabWidget (0,"filters",Qt::WDestructiveClose | Qt::WStyle_DialogBorder);
      fDialog = tabView;
   }
   TIter nextFilter(fFilter);
   StFilterABC *filter = 0;
   StFilterDialog *dialog = 0;
   while ( (filter = (StFilterABC *)nextFilter()) )  {
      float       *pars    = filter->GetPars();
      const float *defs    = filter->GetDefs();
      const char **namval  = filter->GetNams();
      int flag;
      bool *active = filter->GetActive();
      dialog = new StFilterDialog(filter->GetName(),namval,defs, pars, &flag,active); //, flag);
      if (tabView) {
        dialog->reparent(tabView,QPoint(0,0)); 
        tabView->addTab(dialog,filter->GetName());
        connect(dialog,SIGNAL(destroyed()),tabView,SLOT(close()));
      }
   }
   if ( nFilters == 1) fDialog = dialog;
   connect(fDialog,SIGNAL(destroyed()),this,SLOT(Disconnect()));
   // fDialog->Show();
    fDialog->show();
    fDialog->raise();
}
//_______________________________________________________________________________________
void StEventControlPanel::ShowVolumeBrowser()
{
     // Create ROOT TBrowser for the geometry
   if (fgDispMk) {
      delete fBrowser;
      fBrowser = new TBrowser("STAR",fgDispMk->GetHall(),"Full STAR detector geometry");
      TBrowserImp *imp = fBrowser->GetBrowserImp();
      QWidget *bWidget = ((TQtRootBrowserImp *)imp)->GetBrowserID();
      connect (bWidget,SIGNAL(destroyed()),this,SLOT(DisconnectBrowser()));
   }
}
//_______________________________________________________________________________________
void StEventControlPanel::Disconnect()
{ 
   // Dialog has been closed by user. We should forget its pointer
   fDialog = 0;
}
//_______________________________________________________________________________________
void StEventControlPanel::DisconnectBrowser()
{ 
   // TBrowser has been closed by user. We should forget its pointer
   fBrowser = 0;
}

// StEventControlPanel __StEventControlPanel__;

#endif
