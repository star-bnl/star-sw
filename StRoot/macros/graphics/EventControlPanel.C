//*-- Author :    Valery Fine   14/08/01  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: EventControlPanel.C,v 1.1 2001/08/14 21:03:47 fine Exp $
//

////////////////////////////////////////////////////////////////////////
//
// This macro generates a Controlbar panel: 
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/PadControlPanel.gif" ></P> end_html
//
// To execute an item, click with the left mouse button.
//  
// Just start this macro wheneven you want:
//
//  From Root/Cint macro:
//  --------------------
//   .x PadControlPanel.C
//   .L PadControlPanel.C
// or
//   gROOT->LoadMacro("PadControlPanel.C");
//
//  From the compiled C++ code:
//  --------------------
//   gROOT->LoadMacro("PadControlPanel.C");
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

class StEventControlPanel {
  private:
   TControlBar *fBar;
   
  protected:
   //_______________________________________________________________________________________
   void Bar(const Char_t *buttonName, const Char_t *statement,const Char_t *tipText)
   {   fBar->AddButton(buttonName,statement,tipText); }

  public:
   StEventControlPanel(){ fBar=PadControlPanel();}
  //_______________________________________________________________________________________
   static TControlBar *PadControlPanel(TControlBar *bar=0){
//
// This macro generates a Controlbar panel: 
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/PadControlPanel.gif" ></P> end_html
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
//   gROOT->LoadMacro("PadControlPanel.C");
//
//  From the compiled C++ code:
//  --------------------
//   gROOT->LoadMacro("PadControlPanel.C");
//
   if (bar) delete bar;
   bar = new TControlBar("vertical", "Pad Control Panel");
   const char *listEvents[] = {"StEvent(vo)","StEvent(track)"};
   int size = sizeof(listEvents)/4;
   int i;
   for (i=0;i<size;i++) {
     TString n = "StEventControlPanel::ToggleDisplayName(\"";
     n += listEvents[i];
     n += "\"0;";
     bar->AddButton(listEvents[i],n.Data(),"Element of StEvent geometry");
   }
   bar->AddSeparator();
   bar->AddButton("Print names", "StEventControlPanel::PrintNames();", "Print the names of all active elements");

   bar->Show();
   return bar;
}
//_______________________________________________________________________________________
~StEventControlPanel(){ if(fBar) delete fBar; fBar = 0;}
//_______________________________________________________________________________________
 TControlBar *Bar() const { return fBar;}  
//_______________________________________________________________________________________
static void PrintNames()
{
    StEventDisplayMaker *dsm = (StEventDisplayMaker *)chain->Maker("EventDisplay");
    if (dsm) dsmfDsmk->PrintNames();
}
//_______________________________________________________________________________________
static void ToggleDisplayName(const char *name)
{
  StEventDisplayMaker *dsm = (StEventDisplayMaker *)chain->Maker("EventDisplay");
  TList *l = dsm->GetNameList();
  if (!l || !(o=l->FindObject(name))) dsm->AddName(name);
  else dsm->RemoveName(name);
}
//_______________________________________________________________________________________
};

StEventControlPanel __ee__;
void EventControlPanel(){}

