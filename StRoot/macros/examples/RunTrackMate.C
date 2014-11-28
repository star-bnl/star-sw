//________________________________________________________________________________
void RunTrackMate(Int_t nevents=999,
		  //const char *eventFile1="/star/data46/reco/dAuCombined/ReversedFullField/P04if/2003/055/st_physics_4055005_raw_0010010.event.root",
		  //const char *eventFile2="/star/data46/reco/dAuCombined/ReversedFullField/P04if.ittf/2003/055/st_physics_4055005_raw_0010010.event.root",
		  const char *eventFile1="/star/data07/calib/fisyak/ITTF/Tests/tpt.NoEst/productionHigh_FullField/st_physics_5050060_raw_1010010.event.root",
		  const char *eventFile2="/star/data07/calib/fisyak/ITTF/Tests/ittf.noSvt/productionHigh_FullField/st_physics_5050060_raw_1010010.event.root",
		  const char* fileIndex="TEST",
		  const char* outDir=".")
{
  
  gROOT->LoadMacro("bfc.C");
  TString Chain("in,StEvent,nodefault");
  bfc(-2,Chain.Data(),0,0,0);
  gSystem->Load("StTrackMateMaker");
  cout << "Job will run on    File: " << eventFile1 << endl;
  cout << "Corresponding ITTF File: " << eventFile2 << endl;
  // 1st IOMaker, for tpt file
  StIOMaker* ioMaker1 = new StIOMaker("IO1","r",eventFile1);//,"bfcTree");
//   ioMaker1->SetIOMode("r");
//   ioMaker1->SetBranch("*",0,"0");                 //deactivate all branches
//   ioMaker1->SetBranch("eventBranch",0,"r"); //activate event Branch
  
  // 2nd IOMaker, for ittf file
  StIOMaker* ioMaker2 = new StIOMaker("IO2","r",eventFile2);//,"bfcTree");
//   ioMaker2->SetIOMode("r");
//   ioMaker2->SetBranch("*",0,"0");                 //deactivate all branches
//   ioMaker2->SetBranch("eventBranch",0,"r"); //activate event Branch

  StTrackMateMaker*  goodStuff      = new StTrackMateMaker;
  goodStuff->SetFileIndex(fileIndex);
  goodStuff->SetOutDir(outDir);
  //  chain->SetDEBUG(2);
  // now execute the chain member functions
  
  chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
  if (nevents > 0) chain->EventLoop(1,nevents);
}
