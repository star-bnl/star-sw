//
//
//

void RunTrackMate(Int_t nevents=1,
		  const char *eventFile1="/star/data46/reco/dAuCombined/ReversedFullField/P04if/2003/055/st_physics_4055005_raw_0010010.event.root",
		  //const char *eventFile2="/star/data46/reco/dAuCombined/ReversedFullField/P04if.ittf/2003/055/st_physics_4055005_raw_0010010.event.root",
		  const char* fileIndex="TEST",
		  const char* outDir=".")
{
  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDetectorDbMaker");


  gSystem->Load("StTpcDb");
  gSystem->Load("StEvent");
//   gSystem->Load("StEventMaker"); //not needed if event.root branch present
//  gSystem->Load("StEmcUtil"); 


  gSystem->Load("StTrackMateMaker");
  StChain *chain=0;  
  chain = new StChain("StChain"); 
  chain->SetDebug();
   
  // Now we add Makers to the chain...

  // Use INPUTFILE0 from the scheduler.
  // So the 2nd input file should be obtained via string manipulation
  // changing the directory P04if -> P04if.ittf
  TString eventFile2(eventFile1);
  int index = eventFile2.Index("P04if");
  eventFile2.Insert(index+5,".ittf");
  cout << "Job will run on    File: " << eventFile1 << endl;
  cout << "Corresponding ITTF File: " << eventFile2 << endl;
  // 1st IOMaker, for tpt file
  StIOMaker* ioMaker1 = new StIOMaker("IO1","r",eventFile1,"bfcTree");
  ioMaker1->SetDebug();
  ioMaker1->SetIOMode("r");
  ioMaker1->SetBranch("*",0,"0");                 //deactivate all branches
  ioMaker1->SetBranch("eventBranch",0,"r"); //activate geant Branch

  // 2nd IOMaker, for ittf file
  StIOMaker* ioMaker2 = new StIOMaker("IO2","r",eventFile2.Data(),"bfcTree");
  ioMaker2->SetDebug();
  ioMaker2->SetIOMode("r");
  ioMaker2->SetBranch("*",0,"0");                 //deactivate all branches
  ioMaker2->SetBranch("eventBranch",0,"r"); //activate geant Branch

  StTrackMateMaker*  goodStuff      = new StTrackMateMaker;
  goodStuff->SetFileIndex(fileIndex);
  goodStuff->SetOutDir(outDir);
  goodStuff->SetDebug(2);
  
  // now execute the chain member functions

  chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
    
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && istat!=2) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
   if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
   iev++; goto EventLoop;
 } // Event Loop
  
  chain->Finish(); // This should call the Finish() method in ALL makers,
  return;
}

