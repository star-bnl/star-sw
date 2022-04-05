//******************************************************************************

class StChain;
StChain *chain=0;

void DoFeeTTreeSimu(Int_t nevents=100){
  //char *fname="/star/data22/MC/balewski/pp4run3/minb200-10k.event.root";
char *fname="minb200-100.event.root";
char *fileT="myFeeMC-minb200-100.root"; // output TTree

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
     
if (0){
    cout << " StDBLib mode enabled" << endl;
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
  }

  // Load my makers
  gSystem->Load("EEmc");
  gSystem->Load("EEfeeRaw");
  gSystem->Load("SteemcDb");
  
  // create chain    
  chain = new StChain("bfc"); 
  //chain->SetDebug();
  
  // Now we add Makers to the chain...

  // StIOMaker - to read files ...
  StIOMaker* ioMaker = new StIOMaker();  
  ioMaker->SetFile(fname); 
  //ioMaker->SetDebug();

  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  // StMcEventMaker
  StMcEventMaker *mcEventMaker = new StMcEventMaker();
  
 // My Makers  1
  SteemcDbMaker  *myMk1=new SteemcDbMaker("eemcDBio");
  myMk1->setSectors(1,12);
  myMk1->setTimeStampDay(20021215);  // format: yyyymmdd


  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");

  // My Makers  2 
  St2eemcFeeRawMaker * myMk3=new St2eemcFeeRawMaker("St2feeTTree");
//  myMk3->setDb(myMk1);
  
  // Output TTree
  TFile f(fileT,"RECREATE");
  TTree t("fee","A tree with FEE events"); // define branch
  myMk3->setOutTTree(&t);

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
    t.Print();
    
    f.Write();
    printf("\n\n============== TTree closed =========\n\n");
  
    delete myMk3;
    // delete myMk2;
    delete myMk1;

    
}
//******************************************************************************




