//******************************************************************************

class StChain;
StChain *chain=0;

void DoTrigSimu(Int_t nevents=5,char *fname="minb200-10k.event.root")
{
  char *fname="st_physics_4095050_raw_0010002.event.root";
  //
  // First load some shared libraries we need
  //    
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("St_Tables");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker"); 
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StMagF");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StTpcDb"); 
  gSystem->Load("StEventMaker"); 
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StEmcUtil");    
     
  // Load my makers
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");
    gSystem->Load("StEEmcSimulatorMaker");

  // create chain    
  chain = new StChain("bfc"); 
  //chain->SetDebug();
  
  // Now we add Makers to the chain...

  // StIOMaker - to read files ...
  StIOMaker* ioMaker = new StIOMaker();  
  //ioMaker->SetFile("photon_bemc.event.root"); 
  ioMaker->SetFile(fname); 
  //ioMaker->SetDebug();
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  // StMcEventMaker
  StMcEventMaker *mcEventMaker = new StMcEventMaker();

  // My Makers 
  //  SteemcDbMaker  *myMk1=new SteemcDbMaker("eemcDBio");
  StEEmcTrigSimuMaker *myMk2=new StEEmcTrigSimuMaker("eemcTrigMaker");

  // Now execute the chain Init functions
  chain->PrintInfo();
  chain->ls(3);
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
    
  int istat=0,iev=1;

  // Do the event loop    
  EventLoop: 
    if (iev<=nevents && istat!=2) 
    {
      chain->Clear();
      cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
      istat = chain->Make(iev); // This should call the Make() method in ALL makers
      if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
      if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
      iev++; 

 
      goto EventLoop;
    } // Event Loop
    chain->Finish();

    delete myMk2;
    delete myMk1;

    f.Write();  

    
    printf("\n\n============== and of fill for EEMC FEE tree=========\n\n");
    
}
//******************************************************************************




