// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
#include "/afs/rhic.bnl.gov/star/packages/DEV/StRoot/StarClassLibrary/StTimer.hh"

class StMuDstMaker;

StMuDstMaker* maker;

void exampleFix() {

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

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



