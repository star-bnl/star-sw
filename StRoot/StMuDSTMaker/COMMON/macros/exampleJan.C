// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
#include "/afs/rhic/star/packages/DEV/StRoot/StarClassLibrary/StTimer.hh"

class StMuDstMaker;

StMuDstMaker* maker;

void exampleJan() {
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
  maker = new StMuDstMaker(0,0,"MuDstCommon/central/ReversedFullField/runs/","","MuDst");   // set up maker in read mode
  for (int i=0; i<100; i++) {
    cout << i << " ";
    iret = maker->Make();  // read an event 
    if (iret) break;

    StMuEvent* e = maker->muDst()->event();
    if (e) {
      StL0Trigger &t=e->l0Trigger();
      StEventInfo &info=e->eventInfo();
      printf("EVENT id=%d, runId=%d  unix time=%d trigWord=0x%0x bXing=%d spinBits=%d\n", info.id(), info.runId(), info.time(), t.triggerWord(),t.bunchCrossingId(), t.spinBits());
    }
  }

}



