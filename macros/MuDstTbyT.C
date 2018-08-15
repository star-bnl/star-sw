/*
  root.exe lMuDst.C MuTbyT.C+ MuDstTbyT.C
*/
#if ! defined(__CINT__) && ! defined(__CLING__)
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#endif
//class StMuDstMaker;
StMuDstMaker *muDstMko = 0;
StMuDstMaker *muDstMkn = 0;
void muTbyT(StMuDst *oldMuDst, StMuDst *newMuDst);
void MuDstTbyT(
#if 0
	       const Char_t *oldf="/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root", 
	       const Char_t *newf="/gpfs01/star/pwg/fisyak/Embedding/2016/piNmTsq5PerCentZ6cm/st_physics_adc_17125034_raw_1000007.MuDst.root",
#else
	       const Char_t *oldf="/gpfs01/star/subsys-tpc/fisyak/reco/2018/EastOff/st_physics_adc_19116021_raw_1000030.MuDst.root",
	       const Char_t *newf="/gpfs01/star/subsys-tpc/fisyak/reco/2018/WestOff/st_physics_adc_19116021_raw_1000030.MuDst.root",
#endif
	       const Char_t *out = "") {
  TString fileO(out);
  if (fileO == "") {
    fileO = gSystem->BaseName(oldf);
    fileO.ReplaceAll("MuDst.","");
  }
  TFile *fOut = new TFile(fileO,"recreate");
  TH2D *dX = new TH2D("dX","dX (West - East) versus Z",200,-100,100,200,-1.,1.); 
  TH2D *dY = new TH2D("dY","dY (West - East) versus Z",200,-100,100,200,-1.,1.); 
  TH2D *dZ = new TH2D("dZ","dZ (West - East) versus Z",200,-100,100,200,-1.,1.); 
  muDstMko = new StMuDstMaker(0,0,oldf,"","",99999,"MuDstOld"); // no East
  muDstMkn = new StMuDstMaker(0,0,newf,"","",99999,"MuDstNew"); // no West
  chain->Init();
  StMuDst *oldMuDst = 0;
  StMuDst *newMuDst = 0;
  StMuEvent *oldEv = 0, *newEv = 0;
  Int_t NoGTrkOld = 0, NoGTrkNew = 0;
  Int_t nev = 0;
  Bool_t bothRead = kTRUE;
  Int_t iok = chain->MakeEvent();
  while (! iok) {
    oldMuDst = muDstMko->muDst(); oldMuDst->SetInstance(); oldEv = oldMuDst->event(); NoGTrkOld = oldMuDst->numberOfGlobalTracks(); //cout << "old(West) N global " << NoGTrkOld << endl;
    newMuDst = muDstMkn->muDst(); newMuDst->SetInstance(); newEv = newMuDst->event(); NoGTrkNew = newMuDst->numberOfGlobalTracks(); //cout << "new(East) N global " << NoGTrkNew << endl;
    if (oldEv->runId() != newEv->runId()) break;
    if (oldEv->eventId() == newEv->eventId()) {
      nev++;
      //      muTbyT(oldMuDst, newMuDst);
      oldMuDst->SetInstance();
      StThreeVectorF xyzW = oldEv->primaryVertexPosition(); //cout << "old(West) Vx " << xyzW << endl;
      newMuDst->SetInstance();
      StThreeVectorF xyzE = newEv->primaryVertexPosition(); //cout << "new(East) Vx " << xyzE << endl;
      StThreeVectorF xyzD = (xyzW - xyzE)/2;
      StThreeVectorF xyzA = (xyzW + xyzE)/2;
      dX->Fill(xyzA.z(),xyzD.x());
      dY->Fill(xyzA.z(),xyzD.y());
      dZ->Fill(xyzA.z(),xyzD.z());
      iok = chain->MakeEvent();
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
    }
  }
  fOut->Write();
}
