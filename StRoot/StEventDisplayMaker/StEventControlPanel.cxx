#if R__QT
//*-- Author :    Valery Fine   14/08/01  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: StEventControlPanel.cxx,v 1.1 2003/01/08 03:16:31 fine Exp $
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

class StChain;
class StMaker;
class StEventDisplayMaker;
class StEventDisplayInfo;

#include "StEventControlPanel.h"
#include "StEventDisplayInfo.h"

#include "TROOT.h"
#include "TList.h"

#include "StMaker.h"
#include "StEventDisplayMaker.h"

#include <qbuttongroup.h>
#include <qvbuttongroup.h>
#include <qpushbutton.h>
#include <qobjectlist.h>

StMaker             *StEventControlPanel::fgChain  = 0;
StEventDisplayInfo  *StEventControlPanel::fgHlp    = 0;
StEventDisplayMaker *StEventControlPanel::fgDispMk = 0;

//_______________________________________________________________________________________
StEventControlPanel::StEventControlPanel()
{ 
   TClass *kl = gROOT->GetClass("StChain");
   if (kl && kl->GetClassInfo())
   {
      fgChain  = StMaker::GetChain();
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
QButtonGroup *StEventControlPanel::Bar() const { return fBar;}

//_______________________________________________________________________________________
void StEventControlPanel::Build()
{
   const char *listEvents[] = {
         "dst/point"		      ,"dst/point(id_track,position[0]:position[1]:charge)",
         "dst/primtrk"		   ,"dst/primtrk"			         ,
         "dst/globtrk"		   ,"dst/globtrk"			         ,
         "dst/vertex"		   ,"dst/vertex(vtx_id,x:y:z)"	,
         "All Used Hits"  		,"StEvent(All Used Hits)"  	,
         "All Unused Hits"		,"StEvent(All Unused Hits)"	, 
         "TPC Used Hits"  		,"StEvent(Tpc Used Hits)"	   ,
         "TPC Unused Hits"		,"StEvent(Tpc Unused Hits)"	, 
         "FTPC Used Hits"  	,"StEvent(Ftpc Used Hits)"	   ,
         "FTPC Unused Hits"	,"StEvent(Ftpc Unused Hits)"	, 
         "SVT Used Hits"		,"StEvent(Svt Used Hits)"	   , 
         "SVT Unused Hits"		,"StEvent(Svt Unused Hits)"	, 
         "SSD Used Hits"		,"StEvent(Ssd Used Hits)"	   , 
         "SSD Unused Hits"		,"StEvent(Ssd Unused Hits)"	, 
         "RICH Used Hits" 		,"StEvent(Rich Used Hits)"	   ,
         "RICH Unused Hits"	,"StEvent(Rich Unused Hits)"	, 
         "EMC Used Hits"		,"StEvent(Emc Used Hits)"	   , 
         "EMC Unused Hits"		,"StEvent(Emc Unused Hits)"	, 
         "TOF Used Hits"		,"StEvent(Tof Used Hits)"    	, 
         "TOF Unused Hits"		,"StEvent(Tof Unused Hits)"	, 
         "All Tracks"    		,"StEvent(All Tracks)"	   	,
         "All Track Hits"		,"StEvent(All Track Hits)"	   ,
         "Primary Tracks" 		,"StEvent(Primary Tracks)"	   ,
         "Primary Track Hits"	,"StEvent(Primary Track Hits)", 
         "Kink Tracks"    		,"StEvent(Kink Tracks)"		   ,
         "Kink Track Hits"  	,"StEvent(Kink Track Hits)"   , 
         "V0 Tracks"      		,"StEvent(V0 Tracks)"		   ,
         "V0 Track Hits"		,"StEvent(V0 Track Hits)"	   ,  
         "Xi Tracks"      		,"StEvent(Xi Tracks)"	   	,
         "Xi Track Hits"		,"StEvent(Xi Track Hits)"	   , 
         0};      

      fBar = new QVButtonGroup("Event Control Panel",0,"Event");
      fBar->setCaption("Event Control Panel");
      for (int i=0;listEvents[i];i+=2) {AddButt(listEvents[i],listEvents[i+1]);}
      AddButt("Show selections", "StEventControlPanel::PrintNames();");
      connect(fBar,SIGNAL(clicked(int)),this,SLOT(Clicked(int)));
      printf("  ---- >>> %d \n",fBar->count());

      Refresh();
      Show();
}
//_______________________________________________________________________________________
/* public slot: */ 
void StEventControlPanel::Clicked(int id) 
{ 
   if (!fgDispMk) return;
   QButton *b = fBar->find(id);
   if (b) {
      if (id == fBar->count()-1) {
         PrintNames();
         b->setDown(false);
         return;
      }
      switch ( b->state()) {
         case QButton::On: 
         {
           TList *lis = fgDispMk->GetNameList(); 
           if (!lis || !(lis->FindObject(b->name()))) 
              fgDispMk->AddName(b->name());
           break;
         }
         case QButton::Off:
            fgDispMk->RemoveName(b->name());
            break;
         case QButton::NoChange:
            break;
//         case default:
//            break;
      };
   }
}  

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
   if (!tl) return;
   QButton *but=0;
   TObject *n=0;
   QObjectList *l = fBar->queryList( "QButton" );
   QObjectListIt nextButton( *l ); // iterate over the buttons
   while ( (but = (QPushButton *)nextButton.current())) {
      ++nextButton;
      TListIter nextParameter(tl);
      while ((n=nextParameter())) {
         if (strchr(n->GetName(),'(')==0) continue;
         if (strstr(but->name(),n->GetName())) break; 
      }
      if (n ) {
         if (!but->isDown()) but->setDown(true);
      } else {
         if (but->isDown()) but->setDown(false);
      }
   }
   delete l;
   //  Show();
}
//_______________________________________________________________________________________
void StEventControlPanel::AddFilter(TObject *filter)
{   
   char cbuf[100];
   sprintf(cbuf,"((StFilterABC*)%p)->Update();",filter);
   printf("AddFilter: %s::%s\n",filter->GetName(),cbuf);
   //   return AddButt(filter->GetName(), cbuf);
}
//_______________________________________________________________________________________
void StEventControlPanel::Show()
{
   // Show group of buttons.
   fBar->show();
}

// StEventControlPanel __StEventControlPanel__;

#endif
