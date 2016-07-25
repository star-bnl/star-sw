/* 
   root.exe lMuDst.C MuRcPrV.C+
*/
#define __RC__
/* 
   root.exe lMuDst.C MuMcPrV.C+
*/
//#define  __TMVA__
#define __y2012__
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
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#ifndef __RC__
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#endif /* !__RC__ */
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
#include "TMVA/Factory.h"
#include "TMVA/Tools.h"
#ifdef __TMVA__
#include "weights/TMVAClassification_BDT.class.C"
#endif /* __TMVA__ */
#include "TMVAGui.C"
Bool_t iPPV = kFALSE;
struct data_t {Float_t  postx,prompt,cross,tof,notof,BEMC,noBEMC,EEMC,noEEMC,beam,chi2,nWE,zV,zVpd,vR;}; //,Vpd;};
  const Char_t *vnames = "postx:prompt:cross:tof:notof:BEMC:noBEMC:EEMC:noEEMC:beam:chi2:nWE:zV:zVpd:vR"; //:Vpd";
using namespace std;
void TMVAClassification( TString myMethodList = "BDT");
//void TMVAClassification( TString myMethodList = "");
//________________________________________________________________________________
void FillData(data_t &data, const StMuPrimaryVertex *Vtx, Float_t zVpd = -9999) {
  memset(&data.postx, 0, sizeof(data_t));
  Double_t noTracks = Vtx->noTracks();
  if (! noTracks) return; 
  data.beam   =  Vtx->isBeamConstrained() ? 1 : 0;
  data.postx  =  Vtx->nPostXtracks(); // noTracks;
  data.prompt =  Vtx->nPromptTracks(); // noTracks;
  data.cross  =  Vtx->nCrossCentralMembrane(); // noTracks;
  data.tof    = (Vtx->nCTBMatch()     + Vtx->nBTOFMatch()); // noTracks;
  data.notof  = (Vtx->nCTBNotMatch()  + Vtx->nBTOFNotMatch()); // noTracks;
  data.BEMC   =  Vtx->nBEMCMatch(); // noTracks;
  data.noBEMC =  Vtx->nBEMCNotMatch(); // noTracks;
  data.EEMC   =  Vtx->nEEMCMatch(); // noTracks;
  data.noEEMC =  Vtx->nEEMCNotMatch(); // noTracks;
  data.chi2   =  Vtx->chiSquared();
  data.nWE    =  0;
  if (Vtx->nTpcWestOnly() > 0 && Vtx->nTpcEastOnly()) 
    data.nWE = TMath::Min(Vtx->nTpcWestOnly(),Vtx->nTpcEastOnly());// noTracks;
  data.zV     =  Vtx->position().z();
  data.vR     =  Vtx->position().perp();
  data.zVpd   =  zVpd;
}
//________________________________________________________________________________
Bool_t Accept(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
#ifndef __RC__
  if (! gTrack->idTruth()) return kFALSE;
#endif /* ! __RC__ */
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 10) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
#ifndef __RC__
  //  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
#endif /* ! __RC__ */
  if (  Vtx->position().perp() > 0.3) return kFALSE;
  if (  Vtx->nCTBMatch()     + Vtx->nBTOFMatch() +
	Vtx->nBEMCMatch()    + Vtx->nEEMCMatch() <= 0) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptVar(TString string) {
    if (iPPV && string == "beam") return kFALSE;
    if (iPPV && string == "chi2") return kFALSE;
    if (string == "notof") return kFALSE;
    if (string == "noBEMC") return kFALSE;
    if (string == "noEEMC") return kFALSE;
    //    if (string == "cross") return kFALSE;
    //    if (string == "EEMC") return kFALSE;
    if (string == "zV") return kFALSE;
    if (string == "zVpd") return kFALSE;
    if (string == "vR") return kFALSE;
    return kTRUE;
}
//________________________________________________________________________________
void ForceAnimate(unsigned int times=0, int msecDelay=0) {
  unsigned int  counter = times;
  while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
}
//________________________________________________________________________________
void MuMcPrV(Long64_t nevent = 999999,
	     const char* file="./*.MuDst.root",
	     const  char* outFile="MuMcPrV9") {
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
  TString CDir(gSystem->pwd());
  if (CDir.Contains("PPV")) iPPV = kTRUE;
  Int_t year = 2012;
  if (CDir.Contains("2011")) year = 2011;
#ifdef __TMVA__
  OutFile += "TMVArank";
  // create a set of variables and declare them to the reader
  // - the variable names must corresponds in name and type to 
  // those given in the weight file(s) that you use
  TString separator(":");
  TString Vnames(vnames);
  TObjArray *array = Vnames.Tokenize(separator);
  
  vector<string> inputVars;
  TIter next(array);
  TObjString *objs;
  while ((objs = (TObjString *) next())) {
    //    cout << objs->GetString() << endl;
    if (! AcceptVar(objs->GetString())) continue;
    inputVars.push_back( objs->GetString().Data() );
  }
  vector<double>* inputVec = new vector<double>( inputVars.size() );
  //  gROOT->LoadMacro("./TMVAClassification_BDT.class.C++");
  IClassifierReader* classReader = new ReadBDT          ( inputVars );

#endif /* __TMVA__ */
  Float_t RankMin = 0;
#ifdef __TMVA__ 
  if (year == 2011) {
    if (iPPV) RankMin = -0.115;// 11 -0.169;// 8 -0.142;// 7 -0.325; // -0.176; // -0.157;
    else      RankMin = -0.212;// 11 -0.224;// 8-0.245;// 7-0.385; // -0.306; // -0.304;
  } else { // 2012
    if (iPPV) RankMin = -0.039;// -0.042; //-0.327;// 11  -0.361;// 10 -0.378; // -0.462;// -0.0712;
    else      RankMin = -0.025;// -0.028; //-0.347;// 11  -0.353;// 10 -0.437;//-0.395;// -0.1010;
  }
#endif /* __TMVA__ */
  OutFile += ".root";
  TFile *fOut = TFile::Open(OutFile,"recreate");
  data_t data;
  const Int_t nMcRecMult = 100;
  TArrayD xMult(nMcRecMult+1);
  xMult[0] = -0.5;
  for (Int_t i = 1; i <= nMcRecMult; i++) {
    if      (xMult[i-1] <  50) xMult[i] = xMult[i-1] +  1; //  1 - 50
    else if (xMult[i-1] < 100) xMult[i] = xMult[i-1] +  2; // 51 - 75
    else if (xMult[i-1] < 200) xMult[i] = xMult[i-1] + 10; // 76 - 85
    else                       xMult[i] = xMult[i-1] +100; // 86 -100
  }
  struct Name_t {
    const Char_t *Name;
    const Char_t *Title;
  };
  enum {NH = 3, NP = 3};
#ifndef __RC__
  const Name_t HCases[NH] = {
    {"Any", "Any vertex matched with MC == 1"},
    {"Good","The best rank vertex with MC == 1"},
    {"Bad", "The best rank vertex with MC != 1"}
  };
  const Name_t Plots[NP] = {
    {"Mult"    ,"the reconstructed (uncorrected) track multiplicity versus Reconstructable multiplicity"},
    {"MultQA"  ,"the reconstructed (corrected for QA) track multiplicity versus Reconstructable multiplicity"},
    {"McRecMul","Reconstructable multiplicity"}
  };
#else /* __RC__ */
  const Name_t HCases[NH] = {
    {"Any", "Any vertex matched with good Vpd"},
    {"Good","The best rank vertex does match good Vpd"},
    {"Bad", "The best rank vertex does not match good Vpd"}
  };
  const Name_t Plots[NP] = {
    {"Mult"    ,"the reconstructed (uncorrected) track multiplicity versus BToF hit multiplicity"},
    {"MultQA"  ,"the reconstructed PV track multiplicity versus BToF hit multiplicity"},
    {"McRecMul","BToF hit multiplicity"}
  };
  TH1D *McRecMulA = new TH1D("McRecMulA","BToF hit multiplicity", nMcRecMult, xMult.GetArray());
  TH1D *McRecMulB = new TH1D("McRecMulB","BToF hit multiplicity for events with good Vertex", nMcRecMult, xMult.GetArray());
  TH1D *McRecMulT = new TH1D("McRecMulT","BToF hit multiplicity for events with good Vpd and Vx matched", nMcRecMult, xMult.GetArray());

#endif /* ! __RC__ */
  /*         h  p  */
  TH1 *hists[3][4];  memset(hists, 0, sizeof(hists));
  for (Int_t h = 0; h < NH; h++) {
    for (Int_t p = 0; p < NP; p++) {
      TString Name(Plots[p].Name); Name += HCases[h].Name;
      TString Title(Plots[p].Title); Title += " for "; Title += HCases[h].Title; Title += " vertex";
      if      (p <  2)  hists[h][p] = new TH2D(Name,Title,nMcRecMult, xMult.GetArray(),nMcRecMult, xMult.GetArray());
      else if (p == 2)  hists[h][p] = new TH1D(Name,Title,nMcRecMult, xMult.GetArray());
    }
  }
  TNtuple *VertexG = new TNtuple("VertexG","good vertex & global params info",vnames);
  TNtuple *VertexB = new TNtuple("VertexB","bad  vertex & global params info",vnames);
  // ----------------------------------------------
  StMuTimer timer;
  timer.start();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,"st:MuDst.root",1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent",
    "PrimaryVertices",
#ifndef __RC__
    "StStMuMcVertex","StStMuMcTrack",
#endif /* !__RC__ */
    "StBTofHit","StBTofHeader"
  }; 
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  TChain *tree = maker->chain();
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
#if 0
    Int_t referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
    if (_debugAsk) cout << " refMult= "<< referenceMultiplicity;
#endif
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (_debugAsk) cout << "\tPrimaryVertices " << NoPrimaryVertices;
#ifndef __RC__
    TClonesArray *MuMcVertices   = mu->mcArray(0); 
    Int_t NoMuMcVertices = MuMcVertices->GetEntriesFast(); if (_debugAsk) cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices;
    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    Int_t NoMuMcTracks = MuMcTracks->GetEntriesFast(); if (_debugAsk) cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks;
    //    const Double_t field = muEvent->magneticField()*kilogauss;
    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    }
    // Count no. track at a vertex with TPC reconstructable traks.
    multimap<Int_t,Int_t> Mc2McHitTracks;
    for (Int_t m = 0; m < NoMuMcTracks; m++) {
      StMuMcTrack *McTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m);
      if (McTrack->No_tpc_hit() < 15) continue;
      Mc2McHitTracks.insert(pair<Int_t,Int_t>(McTrack->IdVx(),McTrack->Id()));
    }
    Int_t NoMcTracksWithHits = Mc2McHitTracks.count(1);
    McRecMulT->Fill(NoMcTracksWithHits);
    // =============  Build map between  Rc and Mc vertices
    map<StMuPrimaryVertex *,StMuMcVertex *> Mc2RcVertices;
#endif /* !__RC__ */
    if (_debugAsk) cout << endl;
#ifdef __RC__
    Int_t NoToFHits = mu->numberOfBTofHit();
    McRecMulA->Fill(NoToFHits);
#endif /* __RC__  */
    TArrayF Ranks(NoPrimaryVertices);
    //Mc: any vertex with MC==1 and highest reconstrated multiplicity. 
    //Rc: any vertex closest to vpdVz
    Int_t lMBest = -1; 
#ifndef __RC__
    Int_t MMult  = -1;
#endif /*! __RC__ */
    Double_t VpdZ = -9999;
#ifdef __RC__
    Int_t noVpdMatches = 0;
    Double_t zDiff = 1e9;
#endif /* __RC__ */
    StBTofHeader* BTofHeader = mu->btofHeader();
    if ( BTofHeader) {
      UInt_t NoWestHits = BTofHeader->numberOfVpdHits(west);
      UInt_t NoEastHits = BTofHeader->numberOfVpdHits(east);
      if ( NoWestHits > 0 &&  NoEastHits > 0) {
	VpdZ = BTofHeader->vpdVz();
      }
    }
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      Ranks[l] = -1e10;
      if (! AcceptVX(Vtx)) continue;
      //      Vtx->Print();
      UShort_t noTracks = Vtx->noTracks();
      if (! noTracks) continue;
      Ranks[l] = Vtx->ranking();
      Int_t idd = Vtx->idTruth();
      // Check Mc
      if (idd > 0 && idd <= NoMuMcVertices) {
	StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idd-1);
	if (mcVertex->Id() != idd) {
	  cout << "Mismatched idTruth " << idd << " and mcVertex Id " <<  mcVertex->Id() 
	       << " The vertex is ignored" <<  endl;
	}
	//      mcVertex->Print();
	Mc2RcVertices[Vtx] = mcVertex;
	if (idd == 1 && MMult < noTracks) {lMBest = l; MMult = noTracks;}
      }
#ifdef __RC__
      idd = -1;
      Double_t dZ = TMath::Abs(Vtx->position().z()-VpdZ);
      if (dZ < 6) idd = 1;
      if (idd == 1 && dZ < zDiff) {lMBest = l; zDiff = dZ; noVpdMatches++;}
#endif /* __RC__ */
      FillData(data,Vtx,VpdZ);
#ifdef __TMVA__
      Float_t *dataArray = &data.postx;
      UInt_t N = inputVars.size();
      for (UInt_t j = 0; j < N; j++) (*inputVec)[j] = dataArray[j];
      Ranks[l] = classReader->GetMvaValue( *inputVec );
#endif /* __TMVA__ */
    }
    Int_t lBest = TMath::LocMax(NoPrimaryVertices, Ranks.GetArray());
    //    if (lBest >= 0 && Ranks[lBest] < RankMin) lBest = -1;
    if (lBest < 0) continue;
#ifdef __RC__
    McRecMulB->Fill(NoToFHits);
    if (TMath::Abs(VpdZ) > 200) continue;
    if (noVpdMatches != 1) continue;
    McRecMulT->Fill(NoToFHits);
#endif /*  __RC__ */
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      //      if (! iPPV && ! Vtx->isBeamConstrained()) continue;
      if (! AcceptVX(Vtx)) continue;
#ifndef __RC__
      StMuMcVertex *mcVertex = Mc2RcVertices[Vtx];
      if (_debugAsk) {
	cout << Form("Vx[%3i]", l) << *Vtx;
	if (mcVertex) {
	  cout << " " << *mcVertex;
	  Int_t NoMcTracksWithHitsatL = Mc2McHitTracks.count(Vtx->idTruth());
	  cout << Form(" No.McTkHit %4i", NoMcTracksWithHitsatL);
	  Int_t IdPar = mcVertex->IdParTrk();
	  if (IdPar > 0 && IdPar <= NoMuMcTracks) {
	    StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(IdPar-1);
	    if (mcTrack && _debugAsk) cout << " " << mcTrack->GeName();
	  }
	}
	if (Ranks[l] != Vtx->ranking()) cout << " new Rank " << Form("%8.2f",Ranks[l]);
	if      (l == lMBest)  cout << "  === M Best === ";
	else if (l ==  lBest)  cout << "  ===   Best === ";
	else                   cout << "                 ";
	if (TMath::Abs(Vtx->position().z()-VpdZ) < 200) cout << Form("VpdZ %8.2f",Vtx->position().z()-VpdZ);;
	cout << endl;
      }
#endif /* ! __RC__ */
      Double_t noTracks = Vtx->noTracks();
      if (noTracks <= 0.0) continue;
#ifndef __RC__	
      FillData(data, Vtx);
#else
      FillData(data, Vtx,VpdZ);
#endif
      if (l == lMBest) {// good
	VertexG->Fill(&data.postx);
      } else { // bad
	VertexB->Fill(&data.postx);
      }
#ifndef __RC__	
      Double_t noTracksQA = noTracks*Vtx->qaTruth()/100.;
#endif /* ! __RC__ */
      Int_t h = -1;
      if (l == lBest) {
	if (l == lMBest) h = 1;
        else             h = 2;
	if (h > 0) {
#ifndef __RC__	  
	  hists[h][0]->Fill(NoMcTracksWithHits,noTracks);
	  hists[h][1]->Fill(NoMcTracksWithHits,noTracksQA);
	  hists[h][2]->Fill(NoMcTracksWithHits);
#else /* __RC__ */
	  hists[h][0]->Fill(NoToFHits,noTracks);
	  hists[h][1]->Fill(NoToFHits,noTracks);
	  hists[h][2]->Fill(NoToFHits);
#endif /* ! __RC__ */
	}
#ifndef __RC__
	hists[0][0]->Fill(NoMcTracksWithHits,noTracks);
	hists[0][1]->Fill(NoMcTracksWithHits,noTracksQA);
	hists[0][2]->Fill(NoMcTracksWithHits);
#else /* __RC__ */
	hists[0][0]->Fill(NoToFHits,noTracks);
	hists[0][1]->Fill(NoToFHits,noTracks);
	hists[0][2]->Fill(NoToFHits);
#endif /* ! __RC__ */
      }
    }
    if (! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
  }
  fOut->Write();
#ifndef __TMVA__
  TMVAClassification();
#endif
}
//________________________________________________________________________________
void TMVAClassification( TString myMethodList) {
  TTree *signal     = (TTree*)gDirectory->Get("VertexG");
  if (! signal) { cout << "No signal TTree" << endl; return;}
  TTree *background = (TTree*)gDirectory->Get("VertexB");
  if (! background) { cout << "No background TTree" << endl; return;}
  Int_t SampleSize    = (Int_t) signal->GetEntries();
  Int_t nTrain_Signal = SampleSize/2;
  Int_t nTest_Signal  = SampleSize/2;
  Int_t BackgroundSize = (Int_t) background->GetEntries();
  Int_t nTrain_Background = BackgroundSize/2;
  Int_t nTest_Background = BackgroundSize/2;
  //---------------------------------------------------------------
  // This loads the library
  TMVA::Tools::Instance();
  
  // Default MVA methods to be trained + tested
  map<string,int> Use;
  
  // --- Cut optimisation
  Use["Cuts"]            = 1;
  Use["CutsD"]           = 1;
  Use["CutsPCA"]         = 0;
  Use["CutsGA"]          = 0;
  Use["CutsSA"]          = 0;
  // 
  // --- 1-dimensional likelihood ("naive Bayes estimator")
  Use["Likelihood"]      = 1;
  Use["LikelihoodD"]     = 0; // the "D" extension indicates decorrelated input variables (see option strings)
  Use["LikelihoodPCA"]   = 1; // the "PCA" extension indicates PCA-transformed input variables (see option strings)
  Use["LikelihoodKDE"]   = 0;
  Use["LikelihoodMIX"]   = 0;
  //
  // --- Mutidimensional likelihood and Nearest-Neighbour methods
  Use["PDERS"]           = 1;
  Use["PDERSD"]          = 0;
  Use["PDERSPCA"]        = 0;
  Use["PDEFoam"]         = 1;
  Use["PDEFoamBoost"]    = 0; // uses generalised MVA method boosting
  Use["KNN"]             = 1; // k-nearest neighbour method
  //
  // --- Linear Discriminant Analysis
  Use["LD"]              = 1; // Linear Discriminant identical to Fisher
  Use["Fisher"]          = 0;
  Use["FisherG"]         = 0;
  Use["BoostedFisher"]   = 0; // uses generalised MVA method boosting
  Use["HMatrix"]         = 0;
  //
  // --- Function Discriminant analysis
  Use["FDA_GA"]          = 1; // minimisation of user-defined function using Genetics Algorithm
  Use["FDA_SA"]          = 0;
  Use["FDA_MC"]          = 0;
  Use["FDA_MT"]          = 0;
  Use["FDA_GAMT"]        = 0;
  Use["FDA_MCMT"]        = 0;
  //
  // --- Neural Networks (all are feed-forward Multilayer Perceptrons)
  Use["MLP"]             = 0; // Recommended ANN
  Use["MLPBFGS"]         = 0; // Recommended ANN with optional training method
  Use["MLPBNN"]          = 1; // Recommended ANN with BFGS training method and bayesian regulator
  Use["CFMlpANN"]        = 0; // Depreciated ANN from ALEPH
  Use["TMlpANN"]         = 0; // ROOT's own ANN
  //
  // --- Support Vector Machine 
  Use["SVM"]             = 1;
  // 
  // --- Boosted Decision Trees
  Use["BDT"]             = 1; // uses Adaptive Boost
  Use["BDTG"]            = 0; // uses Gradient Boost
  Use["BDTB"]            = 0; // uses Bagging
  Use["BDTD"]            = 0; // decorrelation + Adaptive Boost
  Use["BDTF"]            = 0; // allow usage of fisher discriminant for node splitting 
  Use["myBDTD"]          = 1; // mine
  // 
  // --- Friedman's RuleFit method, ie, an optimised series of cuts ("rules")
  Use["RuleFit"]         = 1;
  // ---------------------------------------------------------------
  
  cout << endl;
  cout << "==> Start TMVAClassification" << endl;
  
  // Select methods (don't look at this code - not of interest)
  if (myMethodList != "") {
    for (map<string,int>::iterator it = Use.begin(); it != Use.end(); it++) it->second = 0;
    
    vector<TString> mlist = TMVA::gTools().SplitString( myMethodList, ',' );
    for (UInt_t i=0; i<mlist.size(); i++) {
      string regMethod(mlist[i]);
      
      if (Use.find(regMethod) == Use.end()) {
	cout << "Method \"" << regMethod << "\" not known in TMVA under this name. Choose among the following:" << endl;
	for (map<string,int>::iterator it = Use.begin(); it != Use.end(); it++) cout << it->first << " ";
	cout << endl;
	return;
      }
      Use[regMethod] = 1;
    }
  }
  
  // --------------------------------------------------------------------------------------------------
  
  // --- Here the preparation phase begins
  
  // Create a ROOT output file where TMVA will store ntuples, histograms, etc.
  TString outfileName( "TMVA.root" );
  TFile* outputFile = TFile::Open( outfileName, "RECREATE" );
  
  // Create the factory object. Later you can choose the methods
  // whose performance you'd like to investigate. The factory is 
  // the only TMVA object you have to interact with
  //
  // The first argument is the base of the name of all the
  // weightfiles in the directory weight/
  //
  // The second argument is the output file for the training results
  // All TMVA output can be suppressed by removing the "!" (not) in
  // front of the "Silent" argument in the option string
  TMVA::Factory *factory = new TMVA::Factory( "TMVAClassification", outputFile,
					      "!V:!Silent:Color:DrawProgressBar:Transformations=I;D;P;G,D:AnalysisType=Classification" );
  
  // If you wish to modify default settings
  // (please check "src/Config.h" to see all available global options)
  //    (TMVA::gConfig().GetVariablePlotting()).fTimesRMS = 8.0;
  //    (TMVA::gConfig().GetIONames()).fWeightFileDir = "myWeightDirectory";
  
  // load the signal and background event samples from ROOT trees
  
  cout <<" starts ... " << endl;
  // global event weights per tree (see below for setting event-wise weights)
  //   Float_t w;
  Double_t signalWeight     = 1.0;
  Double_t backgroundWeight = 1.0;
  
  cout <<" signalWeight = " << signalWeight <<" backWeight = " << backgroundWeight << endl;
  factory->AddSignalTree( signal,    signalWeight     );
  factory->AddBackgroundTree( background, backgroundWeight );
  
  TString separator(":");
  TString Vnames(vnames);
  TObjArray *array = Vnames.Tokenize(separator);
  
  vector<string> inputVars;
  TIter next(array);
  TObjString *objs;
  
  while ((objs = (TObjString *) next())) {
    //    cout << objs->GetString() << endl;
    if (! AcceptVar(objs->GetString())) continue;
    factory->AddVariable(objs->GetString(), 'F');
  }
  // This would set individual event weights (the variables defined in the 
  // expression need to exist in the original TTree)
  //    for signal    : factory->SetSignalWeightExpression("weight1*weight2");
  //    for background: factory->SetBackgroundWeightExpression("weight1*weight2");
  // commented JB : 04/26 ??
  //factory->dSetBackgroundWeightExpression("weight");
  
  // Apply additional cuts on the signal and background samples (can be different)
  TCut mycuts = "";
  TCut mycutb = "";
  
  // Tell the factory how to use the training and testing events
  //
  // If no numbers of events are given, half of the events in the tree are used 
  // for training, and the other half for testing:
  //    factory->PrepareTrainingAndTestTree( mycut, "SplitMode=random:!V" );
  // To also specify the number of testing events, use:
  //factory->PrepareTrainingAndTestTree( mycuts,mycutb,"NSigTrain=9000:NBkgTrain=50000:NSigTest=9000:NBkgTest=50000:SplitMode=Random:!V" );
  //   factory->PrepareTrainingAndTestTree( mycuts, mycutb,"nTrain_Signal=3000:nTrain_Background=20000:nTest_Signal=3000:nTest_Background=20000:SplitMode=Random:!V"); // for KFVertex
  TString SampleSet(Form("nTrain_Signal=%d:nTrain_Background=%d:nTest_Signal=%d:nTest_Background=%d:SplitMode=Random:!V",
			 nTrain_Signal,nTrain_Background,nTest_Signal,nTest_Background));
  factory->PrepareTrainingAndTestTree( mycuts, mycutb,SampleSet.Data());
  
  // ---- Book MVA methods
  //
  // Please lookup the various method configuration options in the corresponding cxx files, eg:
  // src/MethoCuts.cxx, etc, or here: http://tmva.sourceforge.net/optionRef.html
  // it is possible to preset ranges in the option string in which the cut optimisation should be done:
  // "...:CutRangeMin[2]=-1:CutRangeMax[2]=1"...", where [2] is the third input variable
  if (Use["Cuts"]) { // Cut optimisation
    SampleSet = Form("!H:!V:FitMethod=MC:EffSel:SampleSize=%d:VarProp=FSmart",SampleSize);
    factory->BookMethod( TMVA::Types::kCuts, "Cuts",SampleSet.Data());
  }
  if (Use["CutsD"]) {
    SampleSet = Form("!H:!V:FitMethod=MC:EffSel:SampleSize=%d:VarProp=FSmart:VarTransform=Decorrelate",SampleSize);
    factory->BookMethod( TMVA::Types::kCuts, "CutsD",SampleSet.Data());
  }
  if (Use["CutsPCA"]) {
    SampleSet = Form("!H:!V:FitMethod=MC:EffSel:SampleSize=%d:VarProp=FSmart:VarTransform=PCA",SampleSize);
    factory->BookMethod( TMVA::Types::kCuts, "CutsPCA",SampleSet.Data());
  }
  if (Use["CutsGA"]) {
    factory->BookMethod( TMVA::Types::kCuts, "CutsGA",
			 "H:!V:FitMethod=GA:CutRangeMin[0]=-10:CutRangeMax[0]=10:VarProp[1]=FMax:EffSel:Steps=30:"
			 "Cycles=3:PopSize=400:SC_steps=10:SC_rate=5:SC_factor=0.95" );
  }
  if (Use["CutsSA"]) {
    factory->BookMethod( TMVA::Types::kCuts, "CutsSA",
			 "!H:!V:FitMethod=SA:EffSel:MaxCalls=150000:KernelTemp=IncAdaptive:InitialTemp=1e+6:"
			 "MinTemp=1e-6:Eps=1e-10:UseDefaultScale" );
  }
  if (Use["Likelihood"]) { // Likelihood ("naive Bayes estimator")
    factory->BookMethod( TMVA::Types::kLikelihood, "Likelihood",
			 "H:!V:TransformOutput:PDFInterpol=Spline2:NSmoothSig[0]=20:NSmoothBkg[0]=20:NSmoothBkg[1]=10:"
			 "NSmooth=1:NAvEvtPerBin=50" );
  }
  if (Use["LikelihoodD"]) { // Decorrelated likelihood
    factory->BookMethod( TMVA::Types::kLikelihood, "LikelihoodD",
			 "!H:!V:TransformOutput:PDFInterpol=Spline2:NSmoothSig[0]=20:NSmoothBkg[0]=20:NSmooth=5:"
			 "NAvEvtPerBin=50:VarTransform=Decorrelate" );
  }  
  if (Use["LikelihoodPCA"]) { // PCA-transformed likelihood
    factory->BookMethod( TMVA::Types::kLikelihood, "LikelihoodPCA",
			 "!H:!V:!TransformOutput:PDFInterpol=Spline2:NSmoothSig[0]=20:NSmoothBkg[0]=20:NSmooth=5:"
			 "NAvEvtPerBin=50:VarTransform=PCA" ); 
  }
  if (Use["LikelihoodKDE"]) { // Use a kernel density estimator to approximate the PDFs
    factory->BookMethod( TMVA::Types::kLikelihood, "LikelihoodKDE",
			 "!H:!V:!TransformOutput:PDFInterpol=KDE:KDEtype=Gauss:KDEiter=Adaptive:KDEFineFactor=0.3:"
			 "KDEborder=None:NAvEvtPerBin=50" ); 
  }
  if (Use["LikelihoodMIX"]) {// Use a variable-dependent mix of splines and kernel density estimator
    factory->BookMethod( TMVA::Types::kLikelihood, "LikelihoodMIX",
			 "!H:!V:!TransformOutput:PDFInterpolSig[0]=KDE:PDFInterpolBkg[0]=KDE:PDFInterpolSig[1]=KDE:"
			 "PDFInterpolBkg[1]=KDE:PDFInterpolSig[2]=Spline2:PDFInterpolBkg[2]=Spline2:"
			 "PDFInterpolSig[3]=Spline2:PDFInterpolBkg[3]=Spline2:KDEtype=Gauss:KDEiter=Nonadaptive:"
			 "KDEborder=None:NAvEvtPerBin=50" ); 
  }
  // Test the multi-dimensional probability density estimator
  // here are the options strings for the MinMax and RMS methods, respectively:
  //      "!H:!V:VolumeRangeMode=MinMax:DeltaFrac=0.2:KernelEstimator=Gauss:GaussSigma=0.3" );
  //      "!H:!V:VolumeRangeMode=RMS:DeltaFrac=3:KernelEstimator=Gauss:GaussSigma=0.3" );
  if (Use["PDERS"]) {
    factory->BookMethod( TMVA::Types::kPDERS, "PDERS",
			 "!H:!V:NormTree=T:VolumeRangeMode=Adaptive:KernelEstimator=Gauss:GaussSigma=0.3:NEventsMin=400:NEventsMax=600" );
  }
  if (Use["PDERSD"]) {
    factory->BookMethod( TMVA::Types::kPDERS, "PDERSD",
			 "!H:!V:VolumeRangeMode=Adaptive:KernelEstimator=Gauss:GaussSigma=0.3:NEventsMin=400:NEventsMax=600"
			 ":VarTransform=Decorrelate" );
  }
  if (Use["PDERSPCA"]) {
    factory->BookMethod( TMVA::Types::kPDERS, "PDERSPCA",
			 "!H:!V:VolumeRangeMode=Adaptive:KernelEstimator=Gauss:GaussSigma=0.3:NEventsMin=400:NEventsMax=600"
			 ":VarTransform=PCA" );
  }
  if (Use["PDEFoam"]) {// Multi-dimensional likelihood estimator using self-adapting phase-space binning
    factory->BookMethod( TMVA::Types::kPDEFoam, "PDEFoam",
			 "!H:!V:SigBgSeparate=F:TailCut=0.001:VolFrac=0.0666:nActiveCells=500:nSampl=2000:nBin=5:"
			 "Nmin=100:Kernel=None:Compress=T" );
  }
  if (Use["PDEFoamBoost"]) {
    factory->BookMethod( TMVA::Types::kPDEFoam, "PDEFoamBoost",
			 "!H:!V:Boost_Num=30:Boost_Transform=linear:SigBgSeparate=F:MaxDepth=4:UseYesNoCell=T:"
			 "DTLogic=MisClassificationError:FillFoamWithOrigWeights=F:TailCut=0:nActiveCells=500:"
			 "nBin=20:Nmin=400:Kernel=None:Compress=T" );
  }
  if (Use["KNN"]) {// K-Nearest Neighbour classifier (KNN)
    factory->BookMethod( TMVA::Types::kKNN, "KNN",
			 "H:nkNN=20:ScaleFrac=0.8:SigmaFact=1.0:Kernel=Gaus:UseKernel=F:UseWeight=T:!Trim" );
  }  
  if (Use["HMatrix"]) {// H-Matrix (chi2-squared) method
    factory->BookMethod( TMVA::Types::kHMatrix, "HMatrix", "!H:!V:VarTransform=None" );
  }  
  if (Use["LD"]) {// Linear discriminant (same as Fisher discriminant)
    factory->BookMethod( TMVA::Types::kLD, "LD", "H:!V:VarTransform=None:CreateMVAPdfs:PDFInterpolMVAPdf=Spline2:"
			 "NbinsMVAPdf=50:NsmoothMVAPdf=10" );
  }
  if (Use["Fisher"]) { // Fisher discriminant (same as LD)
    factory->BookMethod( TMVA::Types::kFisher, "Fisher", "H:!V:Fisher:VarTransform=None:CreateMVAPdfs:"
			 "PDFInterpolMVAPdf=Spline2:NbinsMVAPdf=50:NsmoothMVAPdf=10" );
  }
  if (Use["FisherG"]) {// Fisher with Gauss-transformed input variables
    factory->BookMethod( TMVA::Types::kFisher, "FisherG", "H:!V:VarTransform=Gauss" );
  }  
  if (Use["BoostedFisher"]) { // Composite classifier: ensemble (tree) of boosted Fisher classifiers
    factory->BookMethod( TMVA::Types::kFisher, "BoostedFisher", 
			 "H:!V:Boost_Num=20:Boost_Transform=log:Boost_Type=AdaBoost:Boost_AdaBoostBeta=0.2:!Boost_DetailedMonitoring" );
  }
  if (Use["FDA_MC"]) {// Function discrimination analysis (FDA) -- test of various fitters - the recommended one is Minuit (or GA or SA)
    
    factory->BookMethod( TMVA::Types::kFDA, "FDA_MC",
			 "H:!V:Formula=(0)+(1)*x0+(2)*x1+(3)*x2+(4)*x3:ParRanges=(-1,1);(-10,10);(-10,10);(-10,10);(-10,10):FitMethod=MC:SampleSize=100000:Sigma=0.1" );
  }
  if (Use["FDA_GA"]) {// can also use Simulated Annealing (SA) algorithm (see Cuts_SA options])
    factory->BookMethod( TMVA::Types::kFDA, "FDA_GA",
			 "H:!V:Formula=(0)+(1)*x0+(2)*x1+(3)*x2+(4)*x3:ParRanges=(-1,1);(-10,10);(-10,10);(-10,10);(-10,10):"
			 "FitMethod=GA:PopSize=300:Cycles=3:Steps=20:Trim=True:SaveBestGen=1" );
  }
  if (Use["FDA_SA"]) {// can also use Simulated Annealing (SA) algorithm (see Cuts_SA options])
    factory->BookMethod( TMVA::Types::kFDA, "FDA_SA",
			 "H:!V:Formula=(0)+(1)*x0+(2)*x1+(3)*x2+(4)*x3:ParRanges=(-1,1);(-10,10);(-10,10);(-10,10);(-10,10):"
			 "FitMethod=SA:MaxCalls=15000:KernelTemp=IncAdaptive:InitialTemp=1e+6:MinTemp=1e-6:Eps=1e-10:UseDefaultScale" );
  }
  if (Use["FDA_MT"]) {
    factory->BookMethod( TMVA::Types::kFDA, "FDA_MT",
			 "H:!V:Formula=(0)+(1)*x0+(2)*x1+(3)*x2+(4)*x3:ParRanges=(-1,1);(-10,10);(-10,10);(-10,10);(-10,10):"
			 "FitMethod=MINUIT:ErrorLevel=1:PrintLevel=-1:FitStrategy=2:UseImprove:UseMinos:SetBatch" );
  }
  if (Use["FDA_GAMT"]) {
    factory->BookMethod( TMVA::Types::kFDA, "FDA_GAMT",
			 "H:!V:Formula=(0)+(1)*x0+(2)*x1+(3)*x2+(4)*x3:ParRanges=(-1,1);(-10,10);(-10,10);(-10,10);(-10,10):"
			 "FitMethod=GA:Converger=MINUIT:ErrorLevel=1:PrintLevel=-1:FitStrategy=0:!UseImprove:!UseMinos:SetBatch:"
			 "Cycles=1:PopSize=5:Steps=5:Trim" );
  }
  if (Use["FDA_MCMT"]) {
    factory->BookMethod( TMVA::Types::kFDA, "FDA_MCMT",
			 "H:!V:Formula=(0)+(1)*x0+(2)*x1+(3)*x2+(4)*x3:ParRanges=(-1,1);(-10,10);(-10,10);(-10,10);(-10,10):"
			 "FitMethod=MC:Converger=MINUIT:ErrorLevel=1:PrintLevel=-1:FitStrategy=0:!UseImprove:!UseMinos:SetBatch:"
			 "SampleSize=20" );
  }
  if (Use["MLP"]) {// TMVA ANN: MLP (recommended ANN) -- all ANNs in TMVA are Multilayer Perceptrons
    factory->BookMethod( TMVA::Types::kMLP, "MLP", "H:!V:NeuronType=tanh:VarTransform=N:NCycles=600:HiddenLayers=N+5:TestRate=5:!UseRegulator" );
  }  
  if (Use["MLPBFGS"]) {
    factory->BookMethod( TMVA::Types::kMLP, "MLPBFGS", "H:!V:NeuronType=tanh:VarTransform=N:NCycles=600:HiddenLayers=N+5:TestRate=5:"
			 "TrainingMethod=BFGS:!UseRegulator" );
  }
  if (Use["MLPBNN"]) { // BFGS training with bayesian regulators
    factory->BookMethod( TMVA::Types::kMLP, "MLPBNN", "H:!V:NeuronType=tanh:VarTransform=N:NCycles=600:HiddenLayers=N+5:TestRate=5:"
			 "TrainingMethod=BFGS:UseRegulator" ); 
  }
  if (Use["CFMlpANN"]) { // CF(Clermont-Ferrand)ANN
    factory->BookMethod( TMVA::Types::kCFMlpANN, "CFMlpANN", "!H:!V:NCycles=2000:HiddenLayers=N+1,N"  ); // n_cycles:#nodes:#nodes:...  
  }
  if (Use["TMlpANN"]) {// Tmlp(Root)ANN  // n_cycles:#nodes:#nodes:...
    factory->BookMethod( TMVA::Types::kTMlpANN, "TMlpANN", "!H:!V:NCycles=200:HiddenLayers=N+1,N:LearningMethod=BFGS:ValidationFraction=0.3"  );
  }
  if (Use["SVM"]) { // Support Vector Machine
    factory->BookMethod( TMVA::Types::kSVM, "SVM", "Gamma=0.25:Tol=0.001:VarTransform=Norm" );
  }
  if (Use["BDTG"]) {// Boosted Decision Trees: Gradient Boost
    factory->BookMethod( TMVA::Types::kBDT, "BDTG",
			 "!H:!V:NTrees=1000:BoostType=Grad:Shrinkage=0.10:UseBaggedGrad:GradBaggingFraction=0.5:nCuts=20:NNodesMax=5" );
  }
  if (Use["BDT"])  { // Adaptive Boost
    factory->BookMethod( TMVA::Types::kBDT, "BDT",
			 "!H:!V:NTrees=850:nEventsMin=150:MaxDepth=3:BoostType=AdaBoost:AdaBoostBeta=0.5:SeparationType=GiniIndex:nCuts=20"
			 ":PruneMethod=NoPruning" );
  }
  if (Use["BDTB"]) {// Bagging
    factory->BookMethod( TMVA::Types::kBDT, "BDTB",
			 "!H:!V:NTrees=400:BoostType=Bagging:SeparationType=GiniIndex:nCuts=20:PruneMethod=NoPruning" );
  }
  if (Use["BDTD"]) {// Decorrelation + Adaptive Boost
    factory->BookMethod( TMVA::Types::kBDT, "BDTD",
			 "!H:!V:NTrees=400:nEventsMin=400:MaxDepth=3:BoostType=AdaBoost:SeparationType=GiniIndex:nCuts=20:PruneMethod=NoPruning"
			 ":VarTransform=Decorrelate" );
}
  if (Use["myBDTD"]) { // Decorrelation + Adaptive Boost
    factory->BookMethod( TMVA::Types::kBDT, "BDTDTEST",
			 "!H:!V:NTrees=1000:nEventsMin=400:MaxDepth=6:BoostType=AdaBoost:SeparationType=GiniIndex:nCuts=20:PruneMethod=NoPruning"
			 ":VarTransform=Decorrelate" );
  }
  if (Use["BDTF"]) { // Allow Using Fisher discriminant in node splitting for (strong) linearly correlated variables
    factory->BookMethod( TMVA::Types::kBDT, "BDTMitFisher",
			 "!H:!V:NTrees=50:nEventsMin=150:UseFisherCuts:MaxDepth=3:BoostType=AdaBoost:AdaBoostBeta=0.5:SeparationType=GiniIndex"
			 ":nCuts=20:PruneMethod=NoPruning" );
  }
  if (Use["RuleFit"]) { // RuleFit -- TMVA implementation of Friedman's method
    factory->BookMethod( TMVA::Types::kRuleFit, "RuleFit",
			 "H:!V:RuleFitModule=RFTMVA:Model=ModRuleLinear:MinImp=0.001:RuleMinDist=0.001:NTrees=20:fEventsMin=0.01:fEventsMax=0.5"
			 ":GDTau=-1.0:GDTauPrec=0.01:GDStep=0.01:GDNSteps=10000:GDErrScale=1.02" );
  }
  // For an example of the category classifier usage, see: TMVAClassificationCategory
  //   TMVA::IMethod* category         = factory->BookMethod( TMVA::Types::kCategory,"Category","" );
  // --------------------------------------------------------------------------------------------------
  // ---- Now you can optimize the setting (configuration) of the MVAs using the set of training events
#if 0
  factory->OptimizeAllMethods("SigEffAt001","Scan");
  factory->OptimizeAllMethods("ROCIntegral","GA");
#endif
  // --------------------------------------------------------------------------------------------------
  // ---- Now you can tell the factory to train, test, and evaluate the MVAs
  // Train MVAs using the set of training events
  factory->TrainAllMethods();
  // ---- Evaluate all MVAs using the set of test events
  factory->TestAllMethods();
  // ----- Evaluate and compare performance of all configured MVAs
  factory->EvaluateAllMethods();
  // --------------------------------------------------------------
  
  // Save the output
  outputFile->Close();
  cout << "==> Wrote root file: " << outputFile->GetName() << endl;
  cout << "==> TMVAClassification is done!" << endl;
  delete factory;
#if 0  
  // Launch the GUI for the root macros
  if (!gROOT->IsBatch()) TMVAGui( outfileName );
#endif
}



void MuRcPrV(Long64_t nevent = 999999,
	     const char* file="./*.MuDst.root",
	     const  char* outFile="MuRcPrV11") {
  MuMcPrV(nevent,file,outFile);
}
