#include "Riostream.h"
#include "TMath.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "TClonesArray.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
static Int_t _debug = 1;
#define PrPP(B) if (_debug) {cout << (#B) << " = \t" << (B) << endl;}
//________________________________________________________________________________
void MuTbyT() {}
//________________________________________________________________________________
void muTbyT(StMuDst* oldMuDst, StMuDst* newMuDst) {
  oldMuDst->SetInstance();
  StMuEvent *oldEv = oldMuDst->event(); Int_t NoGTrkOld = oldMuDst->numberOfGlobalTracks();
  TObjArray    *oldGlobalTracks  = StMuDst::globalTracks();
  TClonesArray *oldPrimaryTracks = StMuDst::allPrimaryTracks();
  TClonesArray *oldDcaTracks     = StMuDst::covGlobTrack();
  newMuDst->SetInstance();
  StMuEvent *newEv = newMuDst->event(); Int_t NoGTrkNew = newMuDst->numberOfGlobalTracks();
  TObjArray    *newGlobalTracks  = StMuDst::globalTracks();
  TClonesArray *newPrimaryTracks = StMuDst::allPrimaryTracks();
  TClonesArray *newDcaTracks     = StMuDst::covGlobTrack();
  // Match Dca Tracks
  Int_t NdcaOld = oldDcaTracks->GetEntriesFast();
  Int_t NdcaNew = newDcaTracks->GetEntriesFast();
  cout << "run = " << oldEv->runId() << "\tevent = " << oldEv->eventId() << " has matched" 
       << " with no. of dca tracks old / new = " << NdcaOld << "/" << NdcaNew	  << endl;
  for (Int_t i = 0; i < NdcaOld; i++) {
    StDcaGeometry* dcaOld = (StDcaGeometry* ) oldDcaTracks->UncheckedAt(i);
    TRVector parOld(5,dcaOld->params());
    TRSymMatrix Cold(5,dcaOld->errMatrix());
    for (Int_t j = 0; j < NdcaNew; j++) {
      StDcaGeometry* dcaNew = (StDcaGeometry* ) newDcaTracks->UncheckedAt(j);
      TRVector parNew(5,dcaNew->params());
      TRSymMatrix Cnew(5,dcaNew->errMatrix());
      TRVector diff(parNew);
      diff -= parOld;
      TRSymMatrix C(Cnew); C += Cold;
      Int_t ok = 0;
      for (Int_t p = 0; p < 5; p++) {
	if (diff(p)*diff(p) > 25*C(p,p)) {ok = 1; break;}
      }
      if (ok) continue;
      cout << "i/j" << i << "/" << j << endl;
      PrPP(parOld); PrPP(Cold);
      PrPP(parNew); PrPP(Cnew);
      PrPP(diff); PrPP(C);
      TRSymMatrix G(C,TRArray::kInverted);
      Double_t chi2 = G.Product(diff,TRArray::kATxSxA);
      PrPP(chi2);
    }
  }
}
