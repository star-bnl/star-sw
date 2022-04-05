class  StChain;
StChain *chain;

void StEventChain( char *fname ="/star/data35/reco/productionPP/ReversedFullField/DEV/2004/117/st_physics_5117072_raw_2010005.event.root"){

  fname ="/star/data05/scratch/balewski/mcMinB/minB-P00-1001+.event.root";
  int nevents =2000;
  int total=0;

  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("libPhysics");
  } 

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert(gSystem->Load("StRFEmcTrigger")==0);
  
  chain= new StChain("StChain"); 
  // chain->SetDebug(1);   

  // StIOMaker - to read files ...
  StIOMaker* ioMaker = new StIOMaker();  
  ioMaker->SetFile(fname); 
  //ioMaker->SetDebug();
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  
  StRFEmcTrigMaker *trig = new StRFEmcTrigMaker("RFTrigA"); 
  //  trig->useMuDst();// or ..
  trig->useStEvent();

  // use filtering capabilities
  //trig->requireBBC();

  trig = new StRFEmcTrigMaker("RFTrigB"); 
  trig->useStEvent();
  trig->requireBBC();


  char *outHistFile="aa2";
  StTreeMaker* treeMk = new StTreeMaker("tree",outHistFile);
  treeMk->SetIOMode("w");
  treeMk->SetBranch("histBranch");
  chain->ls(3);
  //chain->PrintInfo();
  chain->Init();
  for (Int_t iev=0;iev<nevents; iev++) {
    if(iev%50==0)cout << "Working on eventNumber " << iev << endl;
    chain->Clear();
    int iret = chain->Make(iev); 
    total++;
    if (iret && iret!=kStErr) {
      cout << "Bad return code!" << endl;
      break;
    }
  } 
  chain->Finish(); 
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;      
}







