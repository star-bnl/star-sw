//*-- Author :    Valery Fine   22/06/99  (E-mail: fine@bnl.gov)
// $Id: PadBrowser.C,v 1.2 1999/06/23 19:25:18 fine Exp $
// $Log: PadBrowser.C,v $
// Revision 1.2  1999/06/23 19:25:18  fine
// Wrong index fixed
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running PadBrowser maker                                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class StPadDisplayMaker;
StPadDisplayMaker *chain = 0;

//_______________________________________________________________________________________
void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StDaqLib");
    gSystem->Load("StPadDisplayMaker");
    gROOT->LoadMacro("PadControlPanel.C");
}
//_______________________________________________________________________________________
class StPadBrowserPanel {
  private:
    TControlBar       *fBar;  
    StPadDisplayMaker *fMaker;
  public:
  StPadBrowserPanel() { fBar=PadBrowserPanel();}
  //_______________________________________________________________________________________
  static TControlBar *PadBrowserPanel(TControlBar *bar=0)
 {
   if (bar) delete bar;
     bar = new TControlBar("vertical", "Pad Browser Control Panel");

  //  bar->AddButton("Init", "StPadBrowserPanel::Init();", "Load share library and initialize the maker");
     bar->AddButton("Add Axice","St_PolyLine3D::Axis();","Add 3D axice to the cuurent TPad view");
     bar->AddButton("Next Event", "StPadBrowserPanel::Make();", "Make one step");
     bar->AddButton("Draw Next Histogram", "StPadBrowserPanel::MakeHists();", "Make one step");
     bar->AddButton("Finish","StPadBrowserPanel::Finish();","Finish job");

     bar->Show(); 

     return bar;
  }
  //_______________________________________________________________________________________
  static void Make(){
     chain->Clear();
     if (chain->Make()>=kStEOF) printf("End of file\n");
     gPad->Update();
  }
  //_______________________________________________________________________________________
  static void MakeHists(){
    static TCanvas *histCanvas = 0;
    static Int_t histCounter = 0;
    if (!histCanvas) histCanvas = new TCanvas("Pads");
    histCanvas->cd();
    if (histCounter >= 12) histCounter = 0;
    histCounter++;
    chain->GetHists(histCounter++)->Draw("cont");
  }

};

//_______________________________________________________________________________________
void PadBrowser(const Int_t Nevents=1,Char_t *infile=0)
{
  Int_t NoEvents = Nevents;
  Char_t *iNfile = infile;
  // define input file
 // daq data
  if (!iNfile) 
    iNfile ="/scr21/ward/990617.33";
//    iNfile ="/star/tpctest/taper1.R0000000033";
  // Dynamically link some shared libs
  if (gClassTable->GetID("StPadDisplayMaker") < 0) Load();

  // Create the main chain object
  chain = new StPadDisplayMaker("PadMonitor");
  chain->SetFileName(iNfile);
  chain->SetDebug();
  chain->MakeDoc();
  chain->PrintInfo();

  // Init the chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  Int_t i=0;
  for (Int_t i =1; i <= NoEvents; i++){
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    gPad->Update();
    printf ("=========================================== Done with Event no. %d\n",i);
  }
  gSystem->Exec("date");
  StPadBrowserPanel *panel = new StPadBrowserPanel();
}

