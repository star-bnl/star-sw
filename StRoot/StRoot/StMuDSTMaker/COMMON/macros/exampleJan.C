// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
#include "/afs/rhic.bnl.gov/star/packages/DEV/StRoot/StarClassLibrary/StTimer.hh"

class StMuDstMaker;

StMuDstMaker* maker;

void exampleJan() {
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
 
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



