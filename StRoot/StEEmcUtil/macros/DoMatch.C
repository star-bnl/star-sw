//******************************************************************************

class StChain;
StChain *chain=0;

void DoMatch(Int_t nevents=110){
  //  char *fname="/star/data16/reco/dAuMinBias/FullField/P03ia/2003/026/st_physics_4026006_raw_0040*.event.root";
  char *fname="/star/data22/MC/balewski/dAu_MC/rcf1197_95_5899evts-a*.event.root";
  TString miniF="dAu1K.eeTree";

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
  gSystem->Load("StEEmcUtil");
  
  // Now we add Makers to the chain...
  
  TFile *f = new TFile(miniF+".root");
  assert(f->IsOpen());
  TTree *t4 = (TTree*)f->Get("EEtree");
  assert(t4);
  // create a pointer to an event object. This will be used
  // to read the branch values.
  EEeventDst *event = new EEeventDst();
  
  //if (gROOT->IsBatch()) return;  new TBrowser();  t4->StartViewer();  return;
  TBranch *br = t4->GetBranch("EEdst");
  br->SetAddress(&event);
  Int_t nevent = (Int_t)t4->GetEntries();

  printf("\n\n XXXXXXXXXXXXXXXXXXXXXXXXXX  eeTree - events XXXXXXXXXX\n");
  
  for (Int_t ie=0;ie<nevent;ie++) {    
    if(ie>=nevents) break;
    //read this branch only
    br->GetEntry(ie);  
    //    event->print();
    printf("\niEve=%d  ---------- ID=%d token=%d timeSTamp=%d\n",ie,event->ID,event->token,event->timeStamp);

  }
  printf("XXXXXXXXXXXXXXXXXXXXXXXXXX  eeTree - done XXXXXXXXXX\n\n");  
  // create chain    
  chain = new StChain("bfc"); 
  //chain->SetDebug();
  StIOMaker* ioMaker = new StIOMaker();  
  ioMaker->SetFile(fname); 
  
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
  
  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  
  chain->PrintInfo();
  chain->ls(3);
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
  
  int istat=0,iev=1;

  printf(" XXXXXXXXXXXXXXXXXXXXXXXXXX  StEvent - events XXXXXXXXXX\n");  
  // Do the event loop    
 EventLoop: 
  if (iev<=nevents && istat!=2) 
    {
      chain->Clear();
      cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
      istat = chain->Make(iev); // This should call the Make() method in ALL makers
      StEvent *stEvent= (StEvent *) chain->GetInputDS("StEvent");
      printf("StEvent time=%d, ID=%d, runID=%d\n",(int)stEvent->time(),(int)stEvent->id(),(int)stEvent->runId());
      
      StEvtHddr* fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
      printf("EvtHddr actual event time stamp= %d, yyyy/mm/dd=%d hh/mm/ss=%d\n",
	     (int)fEvtHddr->GetUTime(),fEvtHddr->GetDate(),fEvtHddr->GetTime());
      StL0Trigger* L0=(StL0Trigger*)stEvent->l0Trigger();
      
      StTriggerId* trg=(StTriggerId*)  stEvent->triggerIdCollection()->nominal();
      
      //printf("L0 token=%d  trig: is(2003)=%d nominal=%d version=%d\n",L0->triggerToken(),trg->isTrigger(2003),trg->version(2003));
      
      if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
      if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
      iev++; 
      goto  EventLoop;
    } // Event Loop
  chain->Finish();
  

}
//******************************************************************************




