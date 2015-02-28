#include <TSystem.h>

int runqa(Int_t run=16020057,Int_t ped=0,Int_t nevents=0,const Char_t *evpdir = "/evp/a/"){
  int day=run/1000;
  LoadLibs();   
  Int_t ierr = 0;
  
  cout << "Constructing the chain" << endl;
  StChain* analysisChain = new StChain("fpsQAChain");
  
  TString dir0 = "MySQL:StarDb";
  TString dir1 = "$STAR/StarDb";
  St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );  
  
  cout << "Constructing StFmsDbMaker" << endl;
  fgtDbMkr = new StFmsDbMaker();
  

  cout << "Constructing the FPS raw daq reader" << endl;
  char filename[200]; 
  sprintf(filename,"%s/%d",evpdir,run);
  //sprintf(filename,"%s/%d_DELETE",evpdir,run);
  daqRdr = new StFpsRawDaqReader( "daqReader", filename);
  //daqRdr->SetDebug();
  
  cout << "Constructing the FPS QA maker" << endl;
  StFpsQaMaker *qaMkr=new StFpsQaMaker("FpsQa");  
  qaMkr->setRun(run);
  qaMkr->setPed(ped);

  cout << "Initializing" << endl;
  ierr = analysisChain->Init();
  
  if( ierr ){
    cout << "Error initializing" << endl;
    return;
  };
  
  if(nevents<=0) nevents = 1<<30; // a big number
  
  cout << "Max nevents = " << nevents << endl;
  for( int i=0; i<nevents && !ierr; ++i ){
    if( i%100 == 0 ) cout << " Eevent= " << i << endl;
    //cout << " Eevent number " << i << endl;
    ierr = analysisChain->Make();    
    analysisChain->Clear();
  };
  cout << "Finish" << endl;
  analysisChain->Finish();
  return 0;
};

// load the shared libraries
void LoadLibs() {
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbBroker");
  gSystem->Load("RTS");
  gSystem->Load("StFmsDbMaker");
  gSystem->Load("StFpsRawDaqReader");
  gSystem->Load("StFpsQaMaker");
};
