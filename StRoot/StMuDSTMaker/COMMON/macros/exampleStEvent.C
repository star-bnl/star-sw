// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;

StMuDstMaker* maker;
StEvent* ev;


void exampleStEvent() {
   if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
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
  gSystem->Load("StMuDSTMaker");

  cout << " loading done " << endl;
  
  StMuDebug::setLevel(0);  // switch of some debug output

  int iret=0;
  maker = new StMuDstMaker(0,0,"MuDST/central/ReversedFullField/runs/","","MuDst.root",1);   // set up maker in read mode
  iret = maker->Make();  // read an event 
  iret = maker->Make();  

  StMuEvent* e = maker->muDst()->event();
  ev = maker->muDst()->createEvent();

  printf("EVENT id=%d, runId=%d  unix time=%d trigWord=0x%0x bXing=%d spinBits=%d nPrim=%d\n", info.id(), info.runId(), info.time(), t.triggerWord(),t.bunchCrossingId(), t.spinBits(),n);


}



