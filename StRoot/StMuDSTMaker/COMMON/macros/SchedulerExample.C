// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
//#include "StRoot/StMuDSTMaker/COMMON/StMuTypes.hh"

class StMiniEventMaker;
class StMuDbReader;
class StChain;


void load() {
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
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  cout << "loding done " << endl;
}


void SchedulerExample(const char* fileList, const char* outFile="SchedulerExample.root") {
  load();

  // create the chain
  StChain * chain = new StChain("StChain"); 
  chain->SetDebug(0);

  // now create StMuDstMaker
  // agruments are:
  // 0  : read mode
  // 0  : name mode (has no effect on read mode) 
  // "" : input directory, ignored when filename or fileList is specified
  // fileList : list of files to read
  // "" : filter 
  // 1e9 : maximum number of files to read
  // MuDstMaker : name of the maker
  //cout << " press any key " << endl; cin.ignore();
  StMuDebug::setLevel(0);
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",fileList,"",10,"MuDstMaker");


  // now add your analysis maker
  SchedulerExample* analysis = new SchedulerExample(outFile);
 
  // Init the chain
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
  
  int iret = 0;
  int iev =0;
  // now loop over events, makers are call in order of creation
  while ( !iret ) {
    cout << "SchedulerExample.C -- Working on eventNumber " << iev++ << endl;
    chain->Clear();
    iret = chain->Make(iev); // This should call the Make() method in ALL makers    
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers

}
    



