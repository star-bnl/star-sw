void load() {
  // Dynamically link needed shared libs
  // gSystem->Load("libStar");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  
  //  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  //  gSystem->Load("StTreeMaker");
  //  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
//   gSystem->Load("StTpcDb");
//   gSystem->Load("StDbUtilities");
   gSystem->Load("StEvent");
//   gSystem->Load("StEventMaker");
//   gSystem->Load("StEventDstMaker"); 
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
//   gSystem->Load("St_emc_Maker");
//   gSystem->Load("StMcEvent"); 
//   gSystem->Load("StMcEventMaker");
//   gSystem->Load("StAssociationMaker");
//   gSystem->Load("StMcAnalysisMaker");
  //  gSystem->Load("StFlowMaker");
  //  gSystem->Load("StFlowTagMaker");
  //  gSystem->Load("StFlowAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  
  cout << "done" << endl;
 }
