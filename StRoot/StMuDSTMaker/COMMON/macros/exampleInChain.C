// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;


void exampleInChain() {
   if (gClassTable->GetID("TTable") < 0) {
     gSystem->Load("libStar");
     gSystem->Load("libPhysics");
   }     
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StMuDSTMaker");

  cout << " loading done " << endl;
  
  StChain* chain = new StChain("StChain"); 
  chain->SetDebug(1);
  
  StMuDebug::setLevel(0);  // switch of some debug output
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"/star/data02/scratch/laue/MuDstTest/","","MuDst.root",2,"MuDst");   // set up maker in read mode
  StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");

  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
  int nevents =1000;
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "Working on eventNumber " << iev << endl;
    chain->Clear();
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
    if (iret) {
      // cout << "Bad return code!" << endl;
      break;
    }
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers
  
}



