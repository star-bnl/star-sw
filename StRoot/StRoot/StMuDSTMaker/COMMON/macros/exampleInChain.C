// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;


void exampleInChain() {
  gROOT->LoadMacro("StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  
  StChain* chain = new StChain("StChain"); 
  chain->SetDebug(1);
  
  StMuDebug::setLevel(0);  // switch of some debug output
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"","auau200.lis","MuDst.root",20,"MuDst");   // set up maker in read mode
  StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");


  StMuDebug::setLevel(1);  // switch on some debug output

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



