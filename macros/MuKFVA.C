// Vertex Analysis
//   root.exe lMuDst.C 'MuKFVA.C+(999999,"/star/data10/VFtest/2007/\*MuDst.root")'

#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include <cstdlib>
#include <string>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "TObjString.h"
#include "TArrayF.h"
#include "TArrayD.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StBTofHeader.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "Names.h"
#include "StBichsel/Bichsel.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else /* defined(__CINT__) && ! defined(__MAKECINT__) */
#ifndef __MAKECINT__
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif /* __MAKECINT__ */
#endif /* !defined(__CINT__) || defined(__MAKECINT__) */
StMuDstMaker* maker = 0;
#include "Ask.h"
//________________________________________________________________________________
void MuKFVA(Long64_t nevent = 999999,
	     const char* file="./*.MuDst.root",
	     const  char* outFile="MuKFVA") {
  // Initialize histograms -----------------------
  /* 
     1. Data sample : pp200 W->e nu with  pile-up corresponding to 1 MHz min. bias events, 50 K event y2011, 10 K event y2012.
     2. Proof of principal: no pile-up for both PPV and KFV
       a.  Reconstructed primary track multiplicity versus corresponding MC "reconstructable"  
       (i.e. in n STAR acceptance,no. TPC MC hits >= 15)  tracks multiplicity.
       b.  Corrected reconstructed primary track multiplicity (i.e. multiplied by  QA/100.) 
       versus corresponding MC "reconstructable"  (i.e. in n STAR acceptance,no. TPC MC hits >= 15)  tracks multiplicity.
       c.  Efficiency primary vertex reconstruction versus  MC "reconstructable"  tracks multiplicity.
     3. With pileup. repeat above (a-c) with old ranking scheme for 
         I. Any                                                 reconstructed primary vertex which is matched with MC trigger vertex (MC = 1)
        II. The best (in sense of ranking) reconstructed primary vertex which is matched with MC trigger vertex (MC = 1)
       III. The best (in sense of ranking) reconstructed primary vertex which is not matched with MC trigger vertex (MC != 1)
     4. With pileup. repeat above (a-c) with new ranking scheme for cases I-III
  */
  TString OutFile(outFile);
#if 0
  Float_t RankMin = 0;
#endif
  OutFile += ".root";
  TFile *fOut = TFile::Open(OutFile,"recreate");
  static Double_t pionM = 0.13956995;
  struct BPoint_t {
    Float_t MultP, q, MultS;
    Float_t dist, proj, Mass, phi, lambda;
  };
  const Char_t *vars = "MulTP:q:MultS:dist:proj:Mass:phi:lambda";
  TNtuple *FitP = new TNtuple("FitP","KFVA",vars);
  BPoint_t B;
  // ----------------------------------------------
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,"st:MuDst.root",1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent",
    "PrimaryVertices",
    "PrimaryTracks",
#if 0
    "GlobalTracks",
    "CovPrimTrack",
    "CovGlobTrack"
#endif
  }; 
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  TChain *tree = maker->chain();
  if (! tree) {cout << "no TTree" << endl; return;}
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  if (nentries < 100) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);

  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    if (_debugAsk) cout << "Read event #" << ev << "\tRun\t" << muEvent->runId() << "\tId: " << muEvent->eventId() << endl;
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  // cout << "\tPrimaryVertices " << NoPrimaryVertices;
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();  // cout << "\tPrimaryTracks " << NoPrimaryTracks;
#if 0
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();  // cout << "\tGlobalTracks " << NoGlobalTracks;
    TClonesArray *CovPrimTrack     = mu->covPrimTrack(); // cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();
    TClonesArray *CovGlobTrack     = mu->covGlobTrack(); // cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();
#endif    
    TArrayF Ranks(NoPrimaryVertices);
    TVector3 xyzP;
    TVector3 xyzD;
    TVector3 pxyzD;
    TLorentzVector pxyzLD;
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      Ranks[l] = -1e10;
      if (! l) {
	xyzP = TVector3(Vtx->position().xyz());
	B.MultP = Vtx->noTracks();
      }  else     {
	B.MultS = Vtx->noTracks();
	xyzD = TVector3(Vtx->position().xyz());
	pxyzD = TVector3();
	pxyzLD = TLorentzVector();
	TVector3 D = xyzD - xyzP; 
	B.dist = D.Mag();
	B.q = 0;
	for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	  StMuTrack *Trk = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	  if (Trk->vertexIndex() != l) continue;
	  B.q += Trk->charge();
	  TVector3 p(Trk->momentum().xyz());
	  TLorentzVector p4;
	  p4.SetXYZM(p.x(),p.y(),p.z(),pionM);
	  pxyzD += p;
	  pxyzLD += p4;
	}
	TVector3 U = D.Unit();
	B.phi = U.Phi();
	TVector3 Up = pxyzD.Unit();
	B.proj = U.Dot(Up);
	B.lambda = TMath::ACos(B.proj);
	B.Mass = pxyzLD.M();
      }
      if (_debugAsk) {
	cout << Form("Vx[%3i]", l) << *Vtx;
	if (l && B.dist < 1.0) {
	  cout << Form(" q:%2f dist:%6.3f proj:%6.3f M:%6.3f phi:%6.3f lambda:%6.3f", B.q,B.dist, B.proj,B.Mass,B.phi,B.lambda);
	}
	cout << endl;	
      }
      if (l) {
	FitP->Fill(&B.MultP);
      }
      UShort_t noTracks = Vtx->noTracks();
      if (! noTracks) continue;
      Ranks[l] = Vtx->ranking();
    }
    Int_t lBest = TMath::LocMax(NoPrimaryVertices, Ranks.GetArray());
    if (lBest < 0) continue;
    if (! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
  }
  fOut->Write();
}

