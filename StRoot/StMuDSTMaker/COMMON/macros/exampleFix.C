// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
#include "/afs/rhic/star/packages/DEV/StRoot/StarClassLibrary/StTimer.hh"

class StMuDstMaker;

StMuDstMaker* maker;

void exampleFix() {
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
  maker = new StMuDstMaker(0,0,"MuDST/central/ReversedFullField/runs/","","MuDst");   // set up maker in read mode
  for (int i=0; i<100; i++) {
    iret = maker->Make();  // read an event 
    if (iret) break;

    StMuDst* dst = maker->muDst();
    dst->fixTrackIndices();
    int n = maker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    for (int i=0; i<n; i++) {
      StMuTrack* primaryTrack = maker->muDst()->primaryTracks(i);     // get pointer to primary track
      printf("momentumPrimary=%8f ",primaryTrack->p().mag());
      if (primaryTrack->globalTrack()) {
	printf("momentumGlobal=%8f ratio=%8f\n",primaryTrack->globalTrack()->p().mag(),primaryTrack->p().mag()/primaryTrack->globalTrack()->p().mag());
      }
    }
  }
}



