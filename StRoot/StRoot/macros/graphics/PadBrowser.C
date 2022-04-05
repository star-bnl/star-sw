//*-- Author :    Valery Fine   22/06/99  (E-mail: fine@bnl.gov)
// $Id: PadBrowser.C,v 1.9 2000/01/27 20:54:09 fine Exp $
// $Log: PadBrowser.C,v $
// Revision 1.9  2000/01/27 20:54:09  fine
// StarClassLibrary has been added
//
// Revision 1.8  1999/12/19 03:15:42  fine
// StEventDisplayMker loaded
//
// Revision 1.7  1999/09/08 23:28:57  fine
// Histogram browser fixed
//
// Revision 1.6  1999/09/08 19:35:19  fine
// PAdMonitor access to DAQ via IO Maker
//
// Revision 1.5  1999/06/29 20:51:26  fine
// St_geom_Maker has been introduced
//
// Revision 1.4  1999/06/25 18:34:00  fine
// MakeDoc was commented out
//
// Revision 1.3  1999/06/24 02:13:59  fine
// New buttons and bug fixes
//
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
class StChain;
StChain *chain = 0;
StPadDisplayMaker *padMonitor;

//_______________________________________________________________________________________
void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_geom_Maker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StPadDisplayMaker");
    gSystem->Load("StEventDisplayMaker");
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
     bar->AddButton("Draw Next Histogram", "StPadBrowserPanel::MakeHists(+1);", "Make one step");
     bar->AddButton("Draw Prev Histogram", "StPadBrowserPanel::MakeHists(-1);", "Make one step");
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
  static void MakeHists(Int_t direction=1,Int_t hId=0){
  // Display 2D histograms for given sector
    static TCanvas *histCanvas = 0;
    static Int_t histCounter = 0;
    if (!histCanvas) histCanvas = new TCanvas("Pads");
    histCanvas->cd();
    histCounter += direction;
    if (histCounter > 24) histCounter = 1;
    if (histCounter < 1)  histCounter = 24;
    padMonitor->GetHists(hId+histCounter)->Draw("cont");
  }
  //_______________________________________________________________________________________
  static void Finish(){
    chain->Clear();
    chain->Finish();
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
    iNfile ="/scr20/fine/datapool/st_physics_0003455_raw_0001.daq";
//    iNfile ="/star/datapool/1/daq/st_physics_0003455_raw_0001.daq";
//    iNfile ="/star/tpctest/taper1.R0000000033";
  // Dynamically link some shared libs
  if (gClassTable->GetID("StPadDisplayMaker") < 0) Load();

  // Create the main chain object
  chain = new StChain("PadBrowser");
  inpMk = new StIOMaker("inputStream","r",iNfile);
  chain->SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
  geomMaker = new St_geom_Maker();
  padMonitor = new StPadDisplayMaker("PadMonitor");
  chain->SetDebug();
//  chain->MakeDoc();
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

