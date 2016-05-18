void loadMuDst() {
#if 0
  // Dynamically link needed shared libs
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");        // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StEvent");
  gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  gSystem->Load("StBichsel");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  gSystem->Load("StDbLib");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StTofUtil");
  gSystem->Load("StPmdUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  
#else
  gROOT->LoadMacro("bfc.C");
  TString Chain("tables,StUtilities,StEvent,Stu,RMuDst,nodefault");
  bfc(-2,Chain,0,0,0);
  gSystem->Load("libEG");
#endif
  cout << " loading of shared libraries done" << endl;
 }
