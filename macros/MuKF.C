// Vertex Analysis
//   root.exe lMuDst.C 'MuKF.C+(999999,"/star/data10/VFtest/2007/\*MuDst.root")'

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
void MuKF(Long64_t nevent = 999999,
	     const char* file="/star/rcf/test/dev/daq_sl302.ittf/Mon/year_2014/production_15GeV_2014/st_physics_15069008_raw_2500008.MuDst.root",
	     const  char* outFile="MuKF") {
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
#if 1
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();  // cout << "\tGlobalTracks " << NoGlobalTracks;
    TClonesArray *CovPrimTrack     = mu->covPrimTrack(); // cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();
    TClonesArray *CovGlobTrack     = mu->covGlobTrack(); // cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();
#endif    
    vector<KFParticle> particles;
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (_debugAsk) {
	cout << Form("Vx[%3i]", l) << *Vtx;
	cout << endl;	
      }
    }
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (! gTrack) continue;
      Short_t id = gTrack->id();
      Int_t kgc = gTrack->index2Cov();
      if (kgc < 0) continue;
      StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
      if (! dcaG) continue;
      Double_t xyzp[6], CovXyzp[21];
      dcaG->GetXYZ(xyzp,CovXyzp);
      static KFPTrack track;
      track.SetParameters(xyzp);
      track.SetCovarianceMatrix(CovXyzp);
      track.SetNDF(1);
      //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
      track.SetID(id);
      Int_t q   = 1;
      Int_t pdg = 211;
      if (dcaG->charge() < 0) {
	q = -1;
	pdg = -211;
      } 
      track.SetCharge(q);
      
      KFParticle particle(track, pdg);
      particle.SetID(id);
      particle.SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
      //      cout << particle << endl;
      particles.push_back(particle);
    }
    Int_t nDaughters = particles.size();
    KFParticle **vDaughters = new KFParticle*[nDaughters];
    TArrayC VtxFlag(nDaughters); Bool_t *vtxFlag = (Bool_t *) VtxFlag.GetArray();
    for (Int_t i = 0; i < nDaughters; i++) {
      vDaughters[i] = &(particles[i]);
    }
    KFVertex Vtx;
    Vtx.ConstructPrimaryVertex((const KFParticle **) vDaughters, nDaughters, vtxFlag);
    cout << "Vtx:\t" << Vtx << endl;
    if	(! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
    delete [] vDaughters;
  }
  fOut->Write();
}

