// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;

StMuDstMaker* maker;

void example() {
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
  if (e) {
  StL0Trigger &t=e->l0Trigger();
  StEventInfo &info=e->eventInfo();
  }

  int n = maker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
  
  for (int i=0; i<n; i++) {
    StMuTrack* primaryTrack = maker->muDst()->primaryTracks(i);     // get pointer to primary track
    printf("momentumPrimary=%8f ",primaryTrack->p().mag());
    if (primaryTrack->globalTrack()) {
      printf("momentumGlobal=%8f ratio=%8f first-helix=%8f  last-outerHelix=%8f  \n",
	    primaryTrack->globalTrack()->p().mag(),
	    primaryTrack->p().mag()/primaryTrack->globalTrack()->p().mag(),
	    (primaryTrack->globalTrack()->firstPoint() - primaryTrack->globalTrack()->helix().origin()).mag(),
	    (primaryTrack->globalTrack()->lastPoint() - primaryTrack->globalTrack()->outerHelix().origin()).mag()
	    );
    }
  }
  printf("EVENT id=%d, runId=%d  unix time=%d trigWord=0x%0x bXing=%d spinBits=%d nPrim=%d\n", info.id(), info.runId(), info.time(), t.triggerWord(),t.bunchCrossingId(), t.spinBits(),n);


}



