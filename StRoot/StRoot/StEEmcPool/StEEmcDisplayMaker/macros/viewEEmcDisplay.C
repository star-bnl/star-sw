class StEEmcDisplay;

TTree         *tree = 0;
TFile         *file = 0;
StEEmcDisplay *display = 0;

TH2F *towers = 0;
TH1F *smdu   = 0;
TH1F *smdv   = 0;

TH2F *pre1   = 0;
TH2F *pre2   = 0;
TH2F *post   = 0;

TEventList *event_list = 0;

Int_t current_event=0;

// ----------------------------------------------------------------------------
void viewEEmcDisplay( const Char_t *fname, const Char_t fopts="" )
{

  if ( !file )
    {
      Load();
      file = new TFile( fname );
      file->cd();
      //      tree=(TTree*)file->("mTree");
      display=new StEEmcDisplay("display");
      //      tree->SetBranchAddress("display",&display);
      mTree->SetBranchAddress("display",&display);
    }
  event_list = new TEventList("event_list","List of user-selected events");
  mTree->Draw(">>event_list","" );
  gStyle->SetPalette(1);
  gStyle->SetHistMinimumZero();
  help();
}

// ----------------------------------------------------------------------------
void help()
{
  std::cout << std::endl;
  std::cout << "available commands"<<std::endl;
  std::cout << "---------------------------------------------------------------------------------------------" << std::endl;
  std::cout << "list_events()         -- to get a list of events available for display" << std::endl;
  std::cout << "list_events(Char_t *) -- list events which satisfy some set of cuts" << std::endl;
  std::cout << std::endl;
  std::cout << "load_event(Int_t)     -- loads specified event"<< std::endl;
  std::cout << "load_next()           -- loads the next event in the event list" << std::endl;
  std::cout << "show_event()          -- displays towers and points for the current event" << std::endl;
  std::cout << "show_sector(Int_t)    -- zooms in on specified sector and shows smd strips/clusters"<< std::endl;
  std::cout << "print(Char_t*)        -- eg print(\".gif\"), print(\".ps\"), print(\".root\"), etc..." <<std::endl;
  std::cout << std::endl;
  std::cout << "help()                -- show this message"<<std::endl;
  std::cout << std::endl;
}

// ----------------------------------------------------------------------------
void load_event( Long64_t i ) { display->clear(); mTree->GetEntry(i); }
// ----------------------------------------------------------------------------
void load_next(){
  if ( current_event > event_list -> GetN() ) current_event = 0;
  Int_t entry = event_list->GetEntry(current_event);
  load_event(entry);
  std::cout << entry << Form("\t") << display->GetTitle()<<endl;
  current_event++;
}
void load_prev()
{
  if ( current_event == 0 ) 
    current_event = event_list->GetN();
  else
    current_event--;
  Int_t entry = event_list->GetEntry(current_event);
  load_event(entry);
  std::cout << entry << Form("\t") << display->GetTitle()<<endl;  
}
// ----------------------------------------------------------------------------
void show_event() { towers=display->DrawPoints("box"); }
// ----------------------------------------------------------------------------
void show_sector( Int_t sector=-1, Option_t *topts="box", Option_t *sopts="" ) 
{
  if ( sector<1||sector>12 )
    {
      std::cout << "specify a sector 1..12" << std::endl;
      return;
    }
  if ( !towers ) show_event();
  Int_t mysector = sector-1;
  Float_t xmin = 5.0 * (Float_t)mysector - 1.0;
  Float_t xmax = xmin+6.0;
  TCanvas *ctowers=display->getEEmc();
  ctowers->cd();
  towers = display->DrawPoints("box");

  //gROOT->GetListOfCanvases()->Modified();  /* refresh canvas */
  towers->GetXaxis()->SetRangeUser(xmin,xmax);
  //  TCanvas *ctowers = (TCanvas*)gROOT->GetListOfCanvases()->At(0);
  if(ctowers)ctowers->Modified();
  smdu = display->DrawClusters(mysector,0,"stats");
  smdv = display->DrawClusters(mysector,1,"stats");  


  // zoom in on pre/postshower if they are in existence
  TCanvas *others[3];
  TH2F    *hothers[3]={pre1,pre2,post};
  others[0]=display->getPre1();
  others[1]=display->getPre2();
  others[2]=display->getPost();  
  for ( Int_t i=0;i<3;i++ )
    {
      if ( others[i] ) others[i]->cd();
      if ( hothers[i] ) _zoom( sector, hothers[i] );
      if ( others[i] ) others[i]->Modified();
    }

}

void _zoom( Int_t sector, TH2 *h )
{
  Int_t mysector = sector-1;
  Float_t xmin = 5.0 * (Float_t)mysector - 1.0;
  Float_t xmax = xmin+6.0;
  h->GetXaxis()->SetRangeUser(xmin,xmax);
}

// ----------------------------------------------------------------------------
void show_layer( const Char_t *name, Int_t sector=0, Option_t *opts="box,text" )
{
  //  if ( sector<1||sector>12 )
  //    {
  //      std::cout << "specify a sector 1..12" << std::endl;
  //      return;
  //    }

  TString myname(name);
  myname.ToLower();
  
  Int_t ilayer=0;
  if ( myname.Contains("pre1") ) ilayer=1;
  if ( myname.Contains("pre2") ) ilayer=2;
  if ( myname.Contains("post") ) ilayer=3;

  if ( myname.Contains("all") )
    {
      show_layer("pre1",sector,opts);
      show_layer("pre2",sector,opts);
      show_layer("post",sector,opts);
      return;
    }

  TH2F *histo=display->DrawLayer( ilayer, opts );
  if ( sector ) {
    Int_t mysector = sector-1;
    Float_t xmin = 5.0 * (Float_t)mysector - 1.0;
    Float_t xmax = xmin+6.0;
    histo->GetXaxis()->SetRangeUser(xmin,xmax);
  }

  if ( ilayer==1 )
    pre1=histo;
  if ( ilayer==2 )
    pre2=histo;
  if ( ilayer==3 )
    post=histo;
  
}
// ----------------------------------------------------------------------------
void print( const Char_t *type )
{
  
  for ( Int_t i=0;i<3;i++ ) {
    TCanvas *ctowers = (TCanvas*)gROOT->GetListOfCanvases()->At(i);
    if ( ctowers )
      ctowers->Print( TString(ctowers->GetName())+type );
  }
  
}
// ----------------------------------------------------------------------------
void list_events()
{

  mTree->Draw(">>event_list","");
  display->clear();

  Long64_t nevents=mTree->GetEntries();
  std::cout << "nentries="<<nevents<<endl;
  for ( Long64_t i=0;i<nevents;i++ )
    {
      mTree->GetEntry(i);
      std::cout << i << Form("\t") << display->GetTitle()<<endl;
    }

}

void list_events( const Char_t *cuts )
{

  mTree->Draw(">>event_list",cuts );

  for ( Int_t i=0;i<event_list->GetN();i++ )
    {
      Int_t entry=event_list->GetEntry(i);
      mTree->GetEntry(i);
      std::cout << entry << Form("\t") << display->GetTitle()<<endl;
    }

}


// ----------------------------------------------------------------------------
void Load()
{
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcSimulatorMaker");

  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcClusterMaker");
  gSystem->Load("StEEmcPointMaker");
  gSystem->Load("StEEmcPi0Mixer");

  gSystem->Load("StEEmcDisplayMaker");

}
