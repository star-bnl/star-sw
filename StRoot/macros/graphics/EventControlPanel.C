//*-- Author :    Valery Fine   14/08/01  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: EventControlPanel.C,v 1.6 2001/09/17 00:05:24 perev Exp $
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
class StEventControlPanel {
public:

   	TGMainFrame *fBar;  
   	TGLayoutHints *fL1;

static StMaker       *fgChain;
static StEventDisplayMaker *fgDispMk;
static TGMainFrame *fgBar;  
static StEventDisplayInfo *fgHlp;  
private:
        Int_t fNum;
	TList *fGarb;

protected:


//_______________________________________________________________________________________
   TGTextButton *AddButt(const Char_t *buttonName, const Char_t *command)
   {   
       TGTextButton *tb = new TGTextButton(fBar,buttonName,command);
       fGarb->Add(tb);
       int col = (fNum&1)*2;
       int row = (fNum/2)*2;
       fNum++;
       UInt_t uHintsNam = kLHintsCenterY | kLHintsFillX;
       TGTableLayoutHints *hi = new TGTableLayoutHints(col,col+1,row,row+1
                                ,uHintsNam,10,10,10);
       fGarb->Add(hi);

       fBar->AddFrame(tb,hi);   
       Show();
       return tb;
   }


public:
   StEventControlPanel()
   { 
     fgChain = 0;  fgDispMk=0; fgHlp = 0; fNum=0;
     fGarb = new TList;
     TClass *kl = gROOT->GetClass("StChain");
     if (kl && kl->GetClassInfo())
     {
       fgChain  = StMaker::GetChain();
       fgDispMk = (StEventDisplayMaker*)fgChain->GetMaker("EventDisplay");
     }

     Build();
   }

//_______________________________________________________________________________________
   void Build(){
//
   const char *listEvents[] = {
      "dst/point"		,"dst/point(id_track,position[0]:position[1]:charge)",
      "dst/primtrk"		,"dst/primtrk"			,
      "dst/globtrk"		,"dst/globtrk"			,
      "dst/vertex"		,"dst/vertex(vtx_id,x:y:z)"	,
      "All Used Hits"  		,"StEvent(All Used Hits)"	,
      "All Unused Hits"		,"StEvent(All Unused Hits)"	, 
      "TPC Used Hits"  		,"StEvent(Tpc Used Hits)"	,
      "TPC Unused Hits"		,"StEvent(Tpc Unused Hits)"	, 
      "FTPC Used Hits"  	,"StEvent(Ftpc Used Hits)"	,
      "FTPC Unused Hits"	,"StEvent(Ftpc Unused Hits)"	, 
      "SVT Used Hits"		,"StEvent(Svt Used Hits)"	, 
      "SVT Unused Hits"		,"StEvent(Svt Unused Hits)"	, 
      "SSD Used Hits"		,"StEvent(Ssd Used Hits)"	, 
      "SSD Unused Hits"		,"StEvent(Ssd Unused Hits)"	, 
      "RICH Used Hits" 		,"StEvent(Rich Used Hits)"	,
      "RICH Unused Hits"	,"StEvent(Rich Unused Hits)"	, 
      "EMC Used Hits"		,"StEvent(Emc Used Hits)"	, 
      "EMC Unused Hits"		,"StEvent(Emc Unused Hits)"	, 
      "TOF Used Hits"		,"StEvent(Tof Used Hits)"	, 
      "TOF Unused Hits"		,"StEvent(Tof Unused Hits)"	, 
      "All Tracks",    		,"StEvent(All Tracks)"		,
      "All Track Hits"		,"StEvent(All Track Hits)"	,
      "Primary Tracks" 		,"StEvent(Primary Tracks)"	,
      "Primary Track Hits"	,"StEvent(Primary Track Hits)"	, 
      "Kink Tracks"    		,"StEvent(Kink Tracks)"		,
      "Kink Track Hits"  	,"StEvent(Kink Track Hits)"	, 
      "V0 Tracks"      		,"StEvent(V0 Tracks)"		,
      "V0 Track Hits"		,"StEvent(V0 Track Hits)"	,  
      "Xi Tracks"      		,"StEvent(Xi Tracks)"		,
      "Xi Track Hits"		,"StEvent(Xi Track Hits)"	, 
      0}; 
      
   fBar = new TGMainFrame(gClient->GetRoot(), 250,550);
   fBar->SetWindowName("EventControl");
   TGTableLayout  *lay = new TGTableLayout(fBar,40,4,0,0);
   fBar->SetLayoutManager(lay);
   fgBar = fBar;



   int i;
   char cbuf[200];
   for (i=0;listEvents[i];i+=2) {
     TGTextButton *button = AddButt(listEvents[i],"");
     sprintf(cbuf,"StEventControlPanel::ToggleDisplayName(\"%s\",(TGTextButton*)%p)"
     ,listEvents[i+1],button);
     button->SetCommand(cbuf);
   }

   AddButt("Show selections", "StEventControlPanel::PrintNames();");
   Refresh();
   Show();
}
//_______________________________________________________________________________________
~StEventControlPanel()
{  
  fGarb->Delete();
  delete fGarb;
  delete fBar;
}

//_______________________________________________________________________________________
static void PrintNames()
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
void Refresh()
{
  if (!fgDispMk) return;
  TList *tl = fgDispMk->GetNameList();
  if (!tl) return;
  TGButton *but=0;TObject *n=0;
  TList *frameList = fBar->GetList();
  TListIter nextFrame(frameList);
  TGFrameElement *fe=0;
  while ((fe=(TGFrameElement*)nextFrame())) {
    but = (TGButton*)fe->fFrame;
    TListIter nextOne(tl);
    while ((n=nextOne())) {
      if (strchr(n->GetName(),'(')==0) continue;
      if (strstr(but->GetCommand(),n->GetName())) break; }
    int state = (n) ? kButtonDown:kButtonUp;
    but->SetState(state);
 }
 Show();
}
//_______________________________________________________________________________________
static void ToggleDisplayName(const char *name,TGTextButton *button)
{
  if (!fgDispMk) return;
  TList *lis = fgDispMk->GetNameList(); 
  if (!lis || !(lis->FindObject(name))) {
       fgDispMk->AddName(name);
       button->SetState(kButtonDown);
    } else {
       fgDispMk->RemoveName(name);
       button->SetState(kButtonUp);
    }
}
//_______________________________________________________________________________________
   TGTextButton *AddFilter(TObject *filter)
{   
  char cbuf[100];
  sprintf(cbuf,"((StFilterABC*)%p)->Update();",filter);
  printf("AddFilter: %s::%s\n",filter->GetName(),cbuf);
  return AddButt(filter->GetName(), cbuf);
}


//_______________________________________________________________________________________
 void Show()
{
   // Show group of buttons.

   fBar->MapSubwindows();
   fBar->Layout();
//   Resize(GetLayoutManager()->GetDefaultSize());
   fBar->MapWindow();
   fBar->MapRaised();
//   fClient->NeedRedraw(this);
}
};

StEventControlPanel __StEventControlPanel__;


