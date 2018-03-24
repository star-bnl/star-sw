/*
  root.exe lMuDst.C MuDstTbyT.C
*/
#if ! defined(__CINT__) && ! defined(__CLING__)
#include "StMuDSTMaker/COMMON/StMuDst.h"
#endif
//class StMuDstMaker;
StMuDstMaker *muDstMko = 0;
StMuDstMaker *muDstMkn = 0;
void MuDstTbyT(const Char_t *oldf="/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root", 
	       const Char_t *newf="/gpfs01/star/pwg/fisyak/Embedding/2016/piNmTsq5PerCentZ6cm/st_physics_adc_17125034_raw_1000007.MuDst.root") {
#if 0
  TFile *fON[2] = {TFile::Open(oldf), TFile::Open(newf)};
  StMuDst *MuDsts[2] = {0};
  TTree *trees[2] = {0};
#endif
  muDstMko = new StMuDstMaker(0,0,oldf,"","",99999,"MuDstOld");
  muDstMkn = new StMuDstMaker(0,0,newf,"","",99999,"MuDstNew");
  chain->Init();
  StMuDst *oldMuDst = 0;
  StMuDst *newMuDst = 0;
  StMuEvent *oldEv = 0, *newEv = 0;
  Int_t NoGTrkOld = 0, NoGTrkNew = 0;
  Int_t nev = 0;
  Bool_t bothRead = kTRUE;
  Int_t iok = chain->MakeEvent();
  while (! iok) {
    oldMuDst = muDstMko->muDst(); oldMuDst->SetInstance(); oldEv = oldMuDst->event(); NoGTrkOld = oldMuDst->numberOfGlobalTracks();
    newMuDst = muDstMkn->muDst(); newMuDst->SetInstance(); newEv = newMuDst->event(); NoGTrkNew = newMuDst->numberOfGlobalTracks();
    if (oldEv->runId() != newEv->runId()) break;
    if (oldEv->eventId() == newEv->eventId()) {
      nev++;
      cout << "event " << nev << "\trun = " << oldEv->runId() << "\tevent = " << oldEv->eventId() << " has matched" << " with no. of global tracks old / new = " << NoGTrkOld << "/" << NoGTrkNew	  << endl;
    } else {
      cout << "event " << nev << "\trun = " << oldEv->runId() << "\tevents Old = " << oldEv->eventId() << "\tnew = " << newEv->eventId() 
	   << " has not matched" << endl;
      if (oldEv->eventId() < newEv->eventId()) {
        muDstMko->Clear(); iok = muDstMko->Make();
	if (iok) break;
      } else {
	muDstMkn->Clear(); iok = muDstMkn->Make();
	if (iok) break;
      }
      continue;
    }
    iok = chain->MakeEvent();
  }
}
