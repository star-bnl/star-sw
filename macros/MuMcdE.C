/* 
   root.exe lMuDst.C MuMcdE.C+
*/
//#define __DEVT__ 
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
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#include "StarRoot/TPolynomial.h"
#include "KFVertex.h"
#include "KFParticle.h"
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
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))x]
class StMuDstMaker;
#endif
#endif
StMuDstMaker* maker = 0;
#include "GePiD.h"
Int_t _debug = 1;
Int_t Debug() {return _debug;}
//________________________________________________________________________________
void MuMcdE(Long64_t nevent = 999999,
	    //	  const char* file="/star/rcf/test/dev/trs_sl302.ittf/Wed/year_2011/pp500_pileup/rcf10100_90_200evts_Wplus_enu.MuDst.root",
	    const char* file="./*.MuDst.root",
	    const char* filter="st:MuDst.root",
	    const  char* outFile="MuMcdE.root") {
#if 0
  for (Int_t i = 0; i < 173; i++) {
    cout << i+1 << "\t: "; Parts[i].Print();
  }
  return;
#endif
  //  int counter=0;
  TString CDir(gSystem->BaseName(gSystem->pwd()));
  //  if (CDir.Contains("devT") && CDir != "devTR") MinNoMcTpcHits = 11;
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,filter,1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  TTree* atree = maker->tree(); 
  if (! atree ) {
    cout << "No MuDst trees found in path " << file << endl;
    return;
  }
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent",
    "PrimaryVertices",
    "PrimaryTracks",
    "GlobalTracks",
    "CovPrimTrack",
    "CovGlobTrack",
    "StStMuMcVertex",
    "StStMuMcTrack"
    ,"KFTracks"
    ,"KFVertices"
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
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
    if (Debug()) {cout << " #" << ev;}
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryVertices " << NoPrimaryVertices;}
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      if (Debug()) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    TClonesArray *CovPrimTrack     = mu->covPrimTrack();          if (Debug()) {cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          if (Debug()) {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}
    TClonesArray *MuMcVertices   = mu->mcArray(0); 
    Int_t NoMuMcVertices = MuMcVertices->GetEntriesFast();        if (Debug()) {cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices;}
    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    Int_t NoMuMcTracks = MuMcTracks->GetEntriesFast();            if (Debug()) {cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks;}
    TClonesArray *KFTracks = mu->KFTracks();
    Int_t NoKFTracks = KFTracks->GetEntriesFast();                if (Debug()) {cout << "\tKFTracks " << NoKFTracks;}
    TClonesArray *KFVertices = mu->KFVertices();
    Int_t NoKFVertices = KFVertices->GetEntriesFast();            if (Debug()) {cout << "\tKFVertices " << NoKFVertices;}
    if (Debug()) {cout << endl;}
    multimap<StMuMcTrack*,StMuTrack*> &McTrack2GlobalTrack = StMuDst::instance()->McTrack2GlobalTrack();
    for (Int_t i = 0; i < NoMuMcTracks; i++) {
      StMuMcTrack *mcTrack = StMuDst::instance()->MCtrack(i);
      if (! mcTrack) continue;
      
      Int_t count = McTrack2GlobalTrack.count(mcTrack);
      if (! count) continue;
      mcTrack->Print();
      Int_t gePid = mcTrack->GePid();
      if (Parts[gePid-1].Id != gePid) {
	cout << "gePid = " << gePid << " != " << Parts[gePid-1].Id << "  is worng" << endl;
	continue;
      }
      Double_t mass = Parts[gePid-1].Mass;
      Double_t charge = TMath::Abs(Parts[gePid-1].Charge);
      Double_t pMC = mcTrack->Pxyz().mag();
      Double_t eMC = TMath::Sqrt(mass*mass + pMC*pMC);
      pair<multimap<StMuMcTrack*,StMuTrack*>::iterator,multimap<StMuMcTrack*,StMuTrack*>::iterator> McTk2RcTk =  McTrack2GlobalTrack.equal_range(mcTrack);
      for (auto it = McTk2RcTk.first; it != McTk2RcTk.second; ++it) {
	StMuTrack *gTrack = (*it).second;
	if (! gTrack) continue;
	Double_t pIn =  charge*gTrack->muHelix().p().mag();
	Double_t pOut =  charge*gTrack->muOuterHelix().p().mag();
	Double_t eIn = TMath::Sqrt(mass*mass + pIn*pIn);
	Double_t eOut = TMath::Sqrt(mass*mass + pOut*pOut);
	Double_t dE = eIn - eOut;
	gTrack->Print();
	cout << "MC mom: " << pMC 
	     << "\tInner q*mom: " << pIn
	     << "\tOuter q*mom: " << pOut << endl;
	
	cout << "MC bgL10: " << TMath::Log10(pMC/mass)
	     << "\tInner bgL10: " << TMath::Log10(pIn/mass) 
	     << "\tOuter bgL10: " << TMath::Log10(pOut/mass) 
	     << "\teMC = " << eMC << "\teIn = " << eIn << "\teOut = " << eOut
	     << "\tdE = " << 1e6*dE << " keV"
	     << endl;
      }
    }
  }
  //  if (fOut) fOut->Write();
}
