// Upsilon analysis
//   root.exe lMuDst.C 'MuKFU.C+(999999,"*MuDst.root")'

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
#include "TArrayC.h"
#include "TArrayF.h"
#include "TArrayD.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFParticle.h"
#include "KFParticle/KFPTrack.h"
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
void MuKFU(Long64_t nevent = 999999,
	   const char* file="*.MuDst.root",
	   const  char* outFile="MuKFU") {
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
  OutFile += ".root";
  TFile *fOut = TFile::Open(OutFile,"recreate");
  TH1D *uMass = new TH1D("uMass","Upsilon mass",100,9.5,10.5);
  TH1D *kMass = new TH1D("kMass","Upsilon mass @VTX",100,9.5,10.5);
  TH1D *lMass = new TH1D("lMass","Upsilon mass",100,9.5,10.5);
  TH1D *upT   = new TH1D("upT","Upsilon pT",100,0.0,10.0);
  TH2D *dpTvspT   = new TH2D("dpTvspT","dpT/pT versus pT",100,0,10.,100,0.0,0.1);
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
#if 1
    "GlobalTracks",
    "CovPrimTrack",
    "CovGlobTrack"
#endif
  }; 
  TParticlePDG* muon = TDatabasePDG::Instance()->GetParticle(-13);
  Double_t mass = muon->Mass();
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  TChain *tree = maker->chain();
  if (! tree) {cout << "no TTree" << endl; return;}
  Long64_t nentries = tree->GetEntries();
  //  nevent = TMath::Min(nevent,nentries);
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
#if 1
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();  // cout << "\tGlobalTracks " << NoGlobalTracks;
    TClonesArray *CovPrimTrack     = mu->covPrimTrack(); // cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();
    TClonesArray *CovGlobTrack     = mu->covGlobTrack(); // cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();
#endif    
    vector<KFParticle> particles;
    if (_debugAsk) {
      for (Int_t l = 0; l < NoPrimaryVertices; l++) {
	StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
	cout << Form("Vx[%3i]", l) << *Vtx;
	cout << endl;	
      }
    }
    if (NoPrimaryVertices != 1) continue;
    StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(0);
    if (! Vtx) continue;
    KFVertex prodVtx;
    prodVtx.SetBeamConstraint(Vtx->position().x(),Vtx->position().y(),Vtx->position().z(),
			      0.1, 0.1, 0.1);
			      //			      Vtx->posError().x(),Vtx->posError().y(),Vtx->posError().z());
    if (_debugAsk) cout << "prodVtx: " << prodVtx << endl;
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (! gTrack) continue;
      Short_t id = gTrack->id();
      Int_t kgc = gTrack->index2Cov();
      if (kgc < 0) continue;
      StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
      if (! dcaG) continue;
      KFParticle particle(dcaG->Particle(kg+1 -13));
      particle.SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
      if (_debugAsk) {
	cout << "dcaG    :    " << *dcaG << endl;
	cout << "Particle:    " << particle << endl;
      }
      particles.push_back(particle);
    }
    Int_t nDaughters = particles.size();
    for (Int_t i = 0; i < nDaughters; i++) {
      KFParticle muP = particles[i];
      if (muP.GetQ() <= 0) continue;
      KFParticle muPk(muP);
      muPk.SetProductionVertex(prodVtx);
      Float_t dpT, pT;
      muPk.GetP(pT, dpT);
      dpTvspT->Fill(pT, dpT/pT);
      TVector3 p3P(muPk.GetPx(), muPk.GetPy(), muPk.GetPz());
      TLorentzVector pP(p3P, TMath::Sqrt(p3P.Mag2() + mass*mass));
      if (_debugAsk) {
	cout << "muPk@Vtx:" << muPk << endl;
      }
      for (Int_t j = 0; j < nDaughters; j++) {
	KFParticle muN = particles[j];
	if (muN.GetQ() >= 0) continue;
	KFParticle muNk(muN);
	muNk.SetProductionVertex(prodVtx);
	TVector3 p3N(muNk.GetPx(), muNk.GetPy(), muNk.GetPz());
	TLorentzVector pN(p3N, TMath::Sqrt(p3N.Mag2() + mass*mass));
	if (_debugAsk) {
	  cout << "muNk@Vtx:" << muNk << endl;
	}
	KFParticle Upsilon(muP);
	Upsilon += muN;
	TLorentzVector pU = pP;
	pU += pN;
	if (_debugAsk) {
	  cout << "muP:      \t" << muP << endl;
	  cout << "muN:      \t" << muN << endl;
	  cout << "Upsilon:  \t" << Upsilon << endl;
	  cout << "pP:\t" << pP.M() << "\t"; pP.Print("");
	  cout << "pN:\t" << pN.M() << "\t";; pN.Print("");
	  cout << "pU:\t" << pU.M() << "\t";; pU.Print("");
	}
	Float_t M, dM;
	Upsilon.GetMass(M,dM);
	uMass->Fill(M);
	Float_t pT, dpT;
	Upsilon.GetPt(pT,dpT);
	upT->Fill(pT);
	lMass->Fill(pU.M());
      }
    }
    if	(! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
  }
  fOut->Write();
}
/*
  TF1 *f = new TF1("f","TMath::Sqrt([0]*[0]+[1]*[1]*x*x)",3,7)
  dpTvspT->FitSlicesY()
  dpTvspT_1->Fit(f,"er","",3,8)


/net/l404/data/fisyak/reco/Efficiencies/2014/Upsilon2SmTsq.Smeared: dpT/pT = 5.92971e-03 + 5.77886e-03 * pT
/net/l404/data/fisyak/reco/Efficiencies/2019/Upsilon2SmTsq.Smeared: dpT/pT = 7.68464e-03 + 4.48289e-03 * pT
/net/l404/data/fisyak/reco/Efficiencies/2021/Upsilon2SmTsq.Smeared: dpT/pT = 7.29367e-03 + 4.64073e-03 * pT

*/
