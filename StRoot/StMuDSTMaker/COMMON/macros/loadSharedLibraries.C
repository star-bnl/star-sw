void loadSharedLibraries() {
  // Dynamically link needed shared libs
  gSystem->Load("libStar");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");        // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  
  cout << " loading of shared libraries done" << endl;
 }
