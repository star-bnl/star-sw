/* 
   root.exe lMuDst.C MuGamma.C+
   root.exe lMuDst.C MuGamma.root
>> .L MuGamma.C+;  Init(0); DrawEff(); DrawQA(); // FPE_OFF
*/
#define __KFVertex__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "SystemOfUnits.h"
#include "TArrayI.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StarRoot/TPolynomial.h"
#ifdef __KFVertex__
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFParticle.h"
#endif
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
#else
#ifndef __MAKECINT__
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif
#endif
#include "Ask.h"
StMuDstMaker* maker = 0;
static Int_t _debug = 0;
class Gamma_t : public TObject {
public:
  StMuTrack pos, neg;
  StDcaGeometry posDca, negDca;
  KFParticle posK, negK;
  KFParticle gamma;
  ClassDef(Gamma_t,1)
};
static Gamma_t *pGamma[4] = {new Gamma_t(),new Gamma_t(),new Gamma_t(),new Gamma_t()};
//________________________________________________________________________________
Int_t Debug() {return _debug;}
//________________________________________________________________________________
Int_t AcceptTrack(StMuTrack *gTrack) {
  Int_t iok = 1;
  if ( gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return iok; // bad fit or short track pointing to EEMC
#if 0
  if ( gTrack->nHitsFit() < 15) return iok;
  if ( gTrack->probPidTraits().dEdxTrackLength() < 40) return iok;
  if ( TMath::Abs(gTrack->nSigmaElectron()) > 3.0) return iok;
  if ( TMath::Abs(gTrack->btofPidTraits().yLocal()) > 1.8) return iok;
  if ( TMath::Abs(gTrack->btofPidTraits().sigmaElectron()) > 3.0) return iok;
#endif
  iok = 0;
  return iok;
}
//________________________________________________________________________________
void MuV0(Long64_t nevent = 999999,
	  const char* file="*.MuDst.root",
	  const char* filter="st:MuDst.root",
	  const  char* outFile="MuV0.root") {
  TFile *fOut = TFile::Open(outFile,"recreate");
  Int_t bufsize= 64000;
  Int_t split  = -2;       // by default, split Event in sub branches
  if (split)  bufsize /= 4;
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  TTree::SetBranchStyle(branchStyle);

  enum {NoV0types = 4};   // gamma  K0s Lambda, LambdaBar
  Int_t pdgV0[NoV0types]   = {  22, 310, 3122, -3122};  // geantI: {gamma    1,K0s   16, Lambda 18,AntiLambda 26}
  enum {NoDtypes = 4};
  Int_t pdgD[2][NoDtypes] = {{-11, 211, 2212,   211},  //         {positron 2,pion+  8, Proton 14,pion+       8}
                              { 11,-211, -211, -2212}}; //         {electron 3,pion-  9, pion-   9,AntiProton 15}
  string V0name[NoV0types] = {"gamma","K0s","Lambda","LambdaBar"};
  TTree *tGamma[NoV0types];
  TH3F *GammaMass[NoV0types];
  for(Int_t i = 0; i < NoV0types; ++i) {
    tGamma[i] = new TTree(V0name[i].c_str(),( "Reconstracted " + V0name[i] + " Vertex").c_str() );
    tGamma[i] ->Branch(V0name[i].c_str(), "Gamma_t", &pGamma[i] );
    Double_t mmin = 
      TDatabasePDG::Instance()->GetParticle(pdgD[0][i])->Mass() + 
      TDatabasePDG::Instance()->GetParticle(pdgD[1][i])->Mass();
    Double_t dM = 2e-3;
    Int_t    nMbins = 400;
    mmin = dM*((Int_t)(mmin/dM));
    Double_t mmax = mmin + nMbins*dM;
    GammaMass[i] = new TH3F( (V0name[i] + "_hist").c_str(), (V0name[i] + "Mass").c_str(),
			     50,  0., 5., // pT
			     10, -1., 1., // eta
			     nMbins,mmin,mmax ); // Eff.Mass
    GammaMass[i]->GetXaxis()->SetTitle("pT [GeV/c]");
    GammaMass[i]->GetYaxis()->SetTitle("#eta");
    GammaMass[i]->GetZaxis()->SetTitle(Form("Eff. Mass (%s %s) [GeV/c^{2}]",
					    TDatabasePDG::Instance()->GetParticle(pdgD[0][i])->GetName(),
					    TDatabasePDG::Instance()->GetParticle(pdgD[1][i])->GetName())  );
    
  }


  TString CDir(gSystem->BaseName(gSystem->pwd()));
  StMuTimer timer;
  timer.start();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,filter,1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent"
    ,"PrimaryVertices"
    ,"PrimaryTracks"
    ,"GlobalTracks"
    ,"CovPrimTrack"
    ,"CovGlobTrack"
#if 0
    ,"StStMuMcVertex"
    ,"StStMuMcTrack"
#endif
    ,"KFTracks"
    ,"KFVertices"
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  TChain *tree = maker->chain();
  if (! tree) {
    cout << "No TTree" << endl;
    return;
  }
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  if (nentries <= 0) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,0)
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
#endif
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
#if 0
    if ( (!muEvent->triggerIdCollection().nominal().isTrigger(340011))
       && !(muEvent->triggerIdCollection().nominal().isTrigger(340001))
       && !(muEvent->triggerIdCollection().nominal().isTrigger(2)) )
      continue;
#endif
    if (Debug()) {cout << " #" << ev;}
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryVertices " << NoPrimaryVertices;}
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      if (Debug()) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          if (Debug()) {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}
    TClonesArray *KFVertices       = mu->KFVertices(); Int_t NoKFVertices = KFVertices->GetEntriesFast();if (Debug()) {cout << "\tNoKFVertices " << NoKFVertices;}
    TClonesArray *KFTracks         = mu->KFTracks();   Int_t NoKFTracks = KFTracks->GetEntriesFast(); if (Debug()) {cout << "\tNoKFTracks " << NoKFTracks;}
    if (Debug())                                                               {cout << endl;}
    const Float_t field = muEvent->magneticField()*kilogauss;
    KFParticle::SetField(field);
    if (Debug()) {
      for (Int_t l = 0; l < NoPrimaryVertices; l++) {
	StMuPrimaryVertex *RcVx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
	if (! RcVx) continue;
	cout << *RcVx << endl;
      }
    }
    StMuPrimaryVertex *RcVx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(0);
    if (! RcVx) continue; 
    KFParticle *KFVx = 0;
    KFParticle cKFVx;
    if (NoKFVertices) {
      for (Int_t m = 0; m < NoKFVertices; m++) {
	KFVx = (KFParticle *) KFVertices->UncheckedAt(m);
	if (! KFVx) continue;
	if (KFVx->Id() == RcVx->id()) {
	  break;
	}
	KFVx = 0;
      }
    } else {
      KFPVertex vert;
      vert.SetXYZ(RcVx->position().x(), RcVx->position().y(), RcVx->position().z());
      vert.SetCovarianceMatrix( RcVx->posError().x()*RcVx->posError().x(),
				0.00,  RcVx->posError().y()*RcVx->posError().y(),
				0.00,  0.00,  RcVx->posError().z()*RcVx->posError().z());
      vert.SetNContributors(RcVx->noTracks());
      vert.SetChi2(RcVx->chiSquared());
      cKFVx = KFParticle(vert);
      if (Debug()) {
	cout << "Vertex Particle cKFVx" << cKFVx << endl;
      }
      KFVx = &cKFVx;
    }
    if (! KFVx) continue;
    // =============  Build map between global and primary tracks 
    TArrayI Gl2Pr(NoGlobalTracks); Int_t *gl2Pr = Gl2Pr.GetArray();
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (AcceptTrack(gTrack)) continue;
      gl2Pr[kg] = kg+1;
    }
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
      if (  pTrack->vertexIndex() != 0) continue;
      Int_t kg = pTrack->index2Global();
      if (kg < 0) continue;
      gl2Pr[kg] = 0;
    }

for ( Int_t iV0types = 0; iV0types < NoV0types; ++iV0types ) {
    const Float_t Mass = TDatabasePDG::Instance()->GetParticle(pdgV0[iV0types])->Mass();
    Int_t iDtypes = iV0types; {
//  for ( Int_t iDtypes = 0; iDtypes < NoDtypes; ++iDtypes ) {
    for (Int_t kgp = 0; kgp < NoGlobalTracks; kgp++) {
      if (Gl2Pr[kgp] == 0) continue;
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kgp);
      if (gTrack->charge() < 0) continue;
      Int_t kgc = gTrack->index2Cov();
      if (kgc < 0) continue;
      StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
      if (! dcaG) continue;
      pGamma[iV0types]->pos = *gTrack;
      pGamma[iV0types]->posDca = *dcaG;
      KFParticle partP = pGamma[iV0types]->posDca.Particle(kgp);
      pGamma[iV0types]->posK.Create(
		 partP.Parameters(), 
		 partP.CovarianceMatrix(), 
		 partP.Q(), 
		 TDatabasePDG::Instance()->GetParticle(pdgD[0][iDtypes])->Mass());      
      pGamma[iV0types]->posK.SetPDG(pdgD[0][iDtypes]);
      pGamma[iV0types]->posK.SetId(partP.Id());

      for (Int_t kgn = 0; kgn < NoGlobalTracks; kgn++) {
	if (Gl2Pr[kgn] == 0) continue;
	StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kgn);
	if (gTrack->charge() > 0) continue;
	kgc = gTrack->index2Cov();
	if (kgc < 0) continue;
	dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
	if (! dcaG) continue;
	pGamma[iV0types]->neg = *gTrack;
	pGamma[iV0types]->negDca = *dcaG;
	KFParticle partN = pGamma[iV0types]->negDca.Particle(kgn);
	pGamma[iV0types]->negK.Create(
		   partN.Parameters(), 
		   partN.CovarianceMatrix(), 
		   partN.Q(), 
		   TDatabasePDG::Instance()->GetParticle(pdgD[1][iDtypes])->Mass());
	pGamma[iV0types]->negK.SetPDG(pdgD[1][iDtypes]);
	pGamma[iV0types]->negK.SetId(partN.Id());
	Int_t NDaughters = 2;
	const KFParticle *vDaughters[2] = {&pGamma[iV0types]->posK,&pGamma[iV0types]->negK};
	KFParticle gamma;
	gamma.Construct(vDaughters,NDaughters);
	Double_t prob = TMath::Prob(gamma.GetChi2(),gamma.GetNDF());
	if (prob < 1.e-2) continue;
        Float_t M, dM;
	if (gamma.GetMass(M, dM)) {
	  continue;
	}
#if 0
	if (Debug()) {
	  cout << "pos.\t" << pGamma[iV0types]->posK << endl;
	  cout << "neg.\t" << pGamma[iV0types]->negK << endl;
	  cout << "gamma\t" << gamma << endl;
	}
	//	gamma.SetNonlinearMassConstraint(Mass);
	//	gamma.SetMassConstraint(Mass);
	gamma.Construct(vDaughters,NDaughters, 0, Mass);
	prob = TMath::Prob(gamma.GetChi2(),gamma.GetNDF());
	if (prob < 1.e-2) continue;
	gamma.SetPDG(22);
	if (Debug()) {
// 	  cout << "pos.\t" << pGamma->posK << endl;
// 	  cout << "neg.\t" << pGamma->negK << endl;
	  cout << "gamma\t" << gamma << endl;
	}
	//	if (dM < 0 || dM > 1) continue;
	//	if (TMath::Abs(M) > 3*dM) continue;
	gamma.SetProductionVertex(*KFVx);
	prob = TMath::Prob(gamma.GetChi2(),gamma.GetNDF());
	if (prob < 1.e-2) continue;
	if (Debug()) {
	  cout << "gamma\t" << gamma << endl;
	}
	gamma.TransportToDecayVertex();
	if (Debug()) {
	  cout << "gamma\t" << gamma << endl;
	}
	pGamma[iV0types]->gamma = gamma;
	if (Debug()) {
	  cout << "pGamma->gamma\t" << pGamma[iV0types]->gamma << endl;
	}
#endif
	Float_t eta, deta; gamma.GetEta(eta,deta);
	Float_t pT, dpT;   gamma.GetPt(pT,dpT);
        GammaMass[iV0types]->Fill(pT,eta,M);
        if (TMath::Abs(M-Mass) > 5*dM) continue;
	tGamma[iV0types]->Fill();
//	if (! gROOT->IsBatch()) {
//	  if (Ask()) return;
//	} else {_debugAsk = 0;}
      }
    }
  }
}
}
  fOut->cd();
//  GammaMass->Draw();
  if (fOut) fOut->Write();
}
