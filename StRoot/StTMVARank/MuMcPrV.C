/* 
   root.exe -q -b lMuDst.C MuMcPrV.C+; root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE)'; root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE,0.1)'

   root.exe TMVA.root  $ROOTROOT/root/tmva/test/TMVAGui.C
PPV: test -f MuMcPrV28TMVARank.root && root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE,0.250)' >& MuMcPrV28TMVAR.log &
KFV: test -f MuMcPrV28TMVARank.root && root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE,0.183)' >& MuMcPrV28TMVAR.log &

*/
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include <cstdlib>
#include <string>
#include "Riostream.h"
#include "Rtypes.h"
#include "TMath.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TAxis.h"
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
#include "TEfficiency.h"
#include "TChain.h"
#include "TString.h"
#include "TStyle.h"
#include "TObjString.h"
#include "TArrayF.h"
#include "TArrayD.h"
#include "TDatabasePDG.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StTMVARank/TMVAdata.h"
#include "StTMVARank/StTMVARanking.h"
#ifndef __RC__
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#endif /* !__RC__ */
#include "StBTofHeader.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "Names.h"
#include "StBichsel/Bichsel.h"
#include "KFParticle/KFVertex.h"
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
Bool_t doPrint = kFALSE;
#define PrP(B)                                   {cout << (#B) << " = \t" << (B) << endl;}				  
#define PrPP(B)      if (doPrint)                {cout << (#B) << " = \t" << (B) << endl;}				  
#define PrPP2(B,C)   if (doPrint) 		 {cout << (#B) << " = \t" << (B) << "\t" << (C) << endl;}		  
#define PrPP3(B,C,D) if (doPrint) 		 {cout << (#B) << " = \t" << (B) << "\t" << (C) << "\t" << (D) <<  endl;}
#define PrPPD(B)     if (doPrint && Debug() > 1) {cout << (#B) << " = \t" << (B) << endl;}                  
#define PrPPDH(B)    if (doPrint && Debug() > 1) {cout << " =================== " << (#B) << " ===================== " << endl;}
#define PrPP2D(B,C)  if (doPrint && Debug() > 1) {cout << (#B) << " = \t" << (B) << "\t" << (C) << endl;}
StMuDstMaker* maker = 0;
#include "Ask.h"
const Char_t *TMVAMethod = "BDT";
TString TMVAMethodE(Form("%s method",TMVAMethod)); 
Int_t iYear = -1;
TH2F *ppvRankC = 0;
static TClonesArray *PrimaryVertices  = 0; static Int_t NoPrimaryVertices = 0;
static TClonesArray *PrimaryTracks    = 0; static Int_t NoPrimaryTracks = 0;
static TClonesArray *GlobalTracks     = 0; static Int_t NoGlobalTracks  = 0;
static TClonesArray *CovPrimTrack     = 0; 
static TClonesArray *CovGlobTrack     = 0;
static TClonesArray *KFVertices       = 0; static Int_t NoKFVertices     = 0;
static TClonesArray *KFTracks         = 0; static Int_t NoKFTracks       = 0;
static TClonesArray *MuMcVertices     = 0; static Int_t NoMuMcVertices   = 0;
static TClonesArray *MuMcTracks       = 0; static Int_t NoMuMcTracks     = 0;
static TFile *fOut = 0;
using namespace std;
enum VertexMatchType {kMc, kMatch, kClone, kLost, kGhost,  kTotalVertexMatchTypes};
const Char_t *VertexMatchNames[kTotalVertexMatchTypes] = {"Mc", "Match", "Clone", "Lost", "Ghost"};
enum VertexType {kPrimary, kSecondary, kGamma, kK0s, kLambda, kAntiLambda, kTotalVertexTypes};
const Char_t *VertexTypeNames[kTotalVertexTypes] = {"Primary", "Secondary", "Gamma", "K0s", "Lambda", "AntiLambda"};
struct MatchType_t {
  VertexMatchType     k;
  const Char_t    *Name;
  const Char_t   *Title;
};
class McVT_t : public TObject {
public:
  StMuMcVertex Vx;
  StMuMcTrack  Tk;
  ClassDef(McVT_t,1)
};
class Mc_t : public TObject {
public:
  McVT_t  Mc;
  ClassDef(Mc_t,1)
};
class Match_t : public TObject {
public:
  McVT_t  Mc;
  KFParticle V;
  KFParticle VD;
  ClassDef(Match_t,1)
};
class Clone_t : public TObject {
public:
  McVT_t  Mc;
  KFParticle V;
  KFParticle VD;
  ClassDef(Clone_t,1)
};
class Lost_t : public TObject {
public:
  McVT_t Mc;
  ClassDef(Lost_t,1)
};
class Ghost_t : public TObject {
public:
  KFParticle V;
  ClassDef(Ghost_t,1)
};
static Mc_t Mc;        Mc_t    *pMc    = &Mc;
static Match_t Match;  Match_t *pMatch = &Match;
static Clone_t Clone;  Clone_t *pClone = &Clone;
static Lost_t  Lost;   Lost_t  *pLost  = &Lost; 
static Ghost_t Ghost;  Ghost_t *pGhost = &Ghost;
//________________________________________________________________________________
static Int_t _debug = 0;
void SetDebug(Int_t k) {_debug = k;}
Int_t Debug() {return _debug;}
//________________________________________________________________________________
void Setup(const Char_t *xmlFile = "") {
  TString CDir(gSystem->pwd());
  if (CDir.Contains("pileup")) {
    TMVAdata::instance()->SetPileUp();
    cout << "PileUp is set" << endl; 
  }
  if (CDir.Contains("PPV")) {
    TMVAdata::instance()->SetPPV();
    cout << "PPV is set" << endl;
    ppvRankC = new TH2F("ppvRankC","PPV Rank  conversion new versus old",2000,-2e6,2e6,2000,-1,1);
  }
  if (CDir.Contains("2011")) iYear = 2011; 
  if (CDir.Contains("2012")) iYear = 2012; 
  cout << "iYear set to " << iYear << endl;
  TMVAdata::instance()->Init();
  /* default : "postx:prompt:beam:cross:tof:notof:BEMC:noBEMC:nWE:chi2"
     KFV : postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE:chi2
  */
  delete StTMVARanking::instance();
  if (! TMVAdata::instance()->PileUp()) {
#if 1
    if (! TMVAdata::instance()->PPV()) new StTMVARanking("prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
    if (! TMVAdata::instance()->PPV()) new StTMVARanking("prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
#else
    if (! TMVAdata::instance()->PPV()) new StTMVARanking("prompt:cross:tof:notof:BEMC:noBEMC:nWE:chi2",xmlFile);
    else                               new StTMVARanking("prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
#endif
  } else {
#if 1
    if (! TMVAdata::instance()->PPV()) new StTMVARanking("postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
    if (! TMVAdata::instance()->PPV()) new StTMVARanking("postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
#else
    if (! TMVAdata::instance()->PPV()) new StTMVARanking("postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
    else                               new StTMVARanking("postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE",xmlFile);
#endif
  }
}
//________________________________________________________________________________
Float_t PPVRank(Float_t RankOld) {
  Float_t Rank = RankOld;
  if      (Rank >  40000) Rank += - 1e6 + 40000;
  else if (Rank < -40000) Rank +=   1e6 - 40000;
  Rank   =  Rank/100000.;
  if (ppvRankC) ppvRankC->Fill(RankOld,Rank);
  return Rank;
}
//________________________________________________________________________________
void FillData(TMVAdata &Data, const StMuPrimaryVertex *Vtx, Float_t zVpd = -9999, StMuMcVertex *mcVertex = 0,Int_t NoMcTracksWithHits = 0) {
  PVgadgets_st &aData = *(Data.GetArray());
  memset(&aData.postx, 0, sizeof(PVgadgets_st));
  Double_t noTracks = Vtx->noTracks();
  if (! noTracks) return; 
  aData.noTracks = noTracks;
  aData.postx  =  Vtx->nPostXtracks(); // noTracks;
  aData.prompt =  Vtx->nPromptTracks(); // noTracks;
  aData.beam   =  Vtx->isBeamConstrained() ? 1 : 0;
  aData.cross  =  Vtx->nCrossCentralMembrane(); // noTracks;
  aData.tof    = (Vtx->nCTBMatch()     + Vtx->nBTOFMatch()); // noTracks;
  aData.notof  = (Vtx->nCTBNotMatch()  + Vtx->nBTOFNotMatch()); // noTracks;
  aData.BEMC   =  Vtx->nBEMCMatch(); // noTracks;
  aData.noBEMC =  Vtx->nBEMCNotMatch(); // noTracks;
  aData.EEMC   =  Vtx->nEEMCMatch(); // noTracks;
  aData.noEEMC =  Vtx->nEEMCNotMatch(); // noTracks;
  aData.iMc    =  Vtx->idTruth();
  aData.EMC    =  aData.BEMC + aData.EEMC;
  aData.noEMC  =  aData.noBEMC + aData.noEEMC;
  aData.chi2   =  Vtx->chiSquared();
  aData.nWE    =  0;
  if (Vtx->nTpcWestOnly() > 0 && Vtx->nTpcEastOnly() > 0) 
    aData.nWE = TMath::Min(Vtx->nTpcWestOnly(),Vtx->nTpcEastOnly());// noTracks;
  aData.xV     =  Vtx->position().x();
  aData.yV     =  Vtx->position().y();
  aData.zV     =  Vtx->position().z();
  if (aData.iMc) {
    if (mcVertex) {
      aData.xMc     =  mcVertex->XyzV().x();
      aData.yMc     =  mcVertex->XyzV().y();
      aData.zMc     =  mcVertex->XyzV().z();
      aData.timebucket = -99999;
      if (! mcVertex->IdParTrk()) aData.timebucket = TMath::Nint(1e7*mcVertex->Time());      
    }
  }
  aData.zVpd   =  zVpd;
  aData.vR     =  Vtx->position().perp();
  Double_t Rank = Vtx->ranking();
  if (TMVAdata::instance()->PPV()) {
    aData.Rank = PPVRank(Rank);
  } else {// Recalculate rank for KFV
    aData.Rank = - aData.chi2;
    static Float_t Wveto = 1;
    static Float_t Wmatch = 4;
    if (aData.beam)  aData.Rank += Wmatch;
    aData.Rank -= Wveto *aData.postx;
    aData.Rank += Wmatch*aData.prompt;
    aData.Rank += Wmatch*aData.cross;
    aData.Rank += Wmatch*aData.tof
      -          Wveto *aData.notof;
    aData.Rank += Wmatch*(aData.BEMC + aData.EEMC);
    aData.Rank -= Wveto *(aData.noBEMC + aData.noEEMC);
    aData.Rank += Wmatch*aData.nWE;
    aData.Rank /= 1000;
  }
  if (aData.Rank < -1) aData.Rank = -1;
  if (aData.Rank >  1) aData.Rank =  1;
  aData.NoMcTracksWithHits = NoMcTracksWithHits;
}
//________________________________________________________________________________
void PrintMcVx(Int_t idVx = 1) {
  if (! MuMcVertices) return;
  if (idVx > 0 && idVx <= MuMcVertices->GetEntriesFast()) {
    StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idVx-1);	
    if (! mcVertex) return;
    if (MuMcTracks) {
      Int_t iMcTk = mcVertex->IdParTrk();
      if (iMcTk > 0 && iMcTk <= MuMcTracks->GetEntriesFast()) {
	StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(iMcTk-1);
	if (mcTrack) {PrPP3(*mcVertex,*mcTrack,mcTrack->GeName());}
	else         {PrPP(*mcVertex);}
      }
    } else {
      PrPP(*mcVertex);
    }
  }
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
Double_t RankMin() {
  Double_t rank = -1;
  const Char_t *histNames[3] = {"RankGood","RankBad","RankBadT"};
  TH1D *hist[3];
  Double_t *dint[3]; memset(dint, 0, sizeof(dint));
  Int_t nx = 0;
  Int_t i1 = 0;
  Int_t i2 = -1;
  Int_t icut = -1;
  for (Int_t h = 0; h < 3; h++) {
    hist[h] = (TH1D*) gDirectory->Get(histNames[h]);
    if (h <2 && ! hist[h]) return rank;
    if (! h) {
      nx = hist[h]->GetNbinsX();
      i2 = nx+1;
      icut = hist[h]->GetXaxis()->FindBin(rank);
      dint[h] = hist[h]->GetIntegral();
      cout << Form("%-20s",histNames[h]) << " Total no. of entries " << hist[h]->Integral(i1,i2) << endl;
    }
  }
  if (i2 < 0) return 0;
  // Find icut from condition that backgound < 1% from signal
  while(1) {
    Double_t b = hist[1]->Integral(icut,i2);
    Double_t s = hist[0]->Integral(icut,i2);
    if (s <= 0) break;
    Double_t r = b/s;
    //    cout << icut << " b = " << b << " s = " << s << " r = " << r << endl;
    if (r < 0.01) break;
    icut++;
    if (icut > nx) break;
  }
  rank = hist[0]->GetXaxis()->GetBinUpEdge(icut);
  Double_t GT = hist[0]->Integral();
  Double_t GC = hist[0]->Integral(icut,i2);
  Double_t BC = hist[1]->Integral(icut,i2);
  Double_t BT = hist[2]->Integral(icut,i2);
  cout << "At rank min = " << rank << " Triggered Vertex Efficiency " << GC/GT << " Good Vertices "
       << "with background from pile-up " << BC/GC << " and the triggered bunch crossing pile-up " << BT/GC << endl;
  return rank;
}
//________________________________________________________________________________
Bool_t AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
  if (Vtx->noTracks() <= 0) return kFALSE;
#ifndef __RC__
  //  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
#endif /* ! __RC__ */
  //10c  if (  Vtx->position().perp() > 0.3) return kFALSE;
#if 0
  //12 | 19
  //  if (  Vtx->nCTBMatch()     + Vtx->nBTOFMatch() +
  //	Vtx->nBEMCMatch()    + Vtx->nEEMCMatch() <= 0) return kFALSE;
  if (  Vtx->nCTBMatch()     + Vtx->nBTOFMatch() +
  	Vtx->nBEMCMatch()    + Vtx->nEEMCMatch() <= 1) return kFALSE; // 22
#endif
  //41  if (! TMVAdata::instance()->PPV() && !  Vtx->isBeamConstrained()) return kFALSE; //20
  //21  if (  Vtx->position().perp() > 3.0) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
void ForceAnimate(UInt_t times=0, Int_t msecDelay=0) {
  UInt_t  counter = times;
  while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
}
//________________________________________________________________________________
void FillTrees(TTree *tree, StMuMcVertex *McVx = 0, StMuMcTrack *McTrack = 0, KFParticle* KFVx = 0) {
  McVT_t MC;
  KFParticle V, VD;
  if (McVx)    MC.Vx = *McVx;
  if (McTrack) MC.Tk = *McTrack;
  if (KFVx)    {
    V     = *KFVx;
    VD    = V;
    VD.TransportToDecayVertex();
  }
  Mc.Mc = MC;
  Match.Mc = MC; Match.V = V; Match.VD = VD;
  Clone.Mc = MC; Clone.V = V; Clone.VD = VD;
  Lost.Mc  = MC;
  Ghost.V  = V;
  tree->Fill();
}
//________________________________________________________________________________
void MuMcPrV(Bool_t iTMVA = kFALSE, Float_t RankMin = 0, Long64_t Nevent = 999999, 
	     const char* file="*.MuDst.root",
	     const  char* outFile="MuMcPrV50") { 
  // 12 only "B"
  // 13 no request for fast detectors, no restriction to beam match but rVx < 3 cm
  // 19 require tof or emc match, QA > 25
  // 20 +require beam for KFV, QA > 50
  // 21 modify rank for KFV (fix bug with vetos)
  // 22 No .of Tracks matched with fast detectors >= 2, no cut on vertex QA, no cut of rank
  // 31
  // 32 take out EEmc
  // 33 an automatic choise of variables based on their RMS
  // 34 use table
  // 36 use table
  // 37 use only "B" for KFV
  // 38 check StTMVAranking
  // 39 switch to maps
  // 40 plots only for l == lBest
  // 41 add beam for KFV vertex selection
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
  if (iYear < 0) {
    TString xmlFile;
    if (iTMVA) xmlFile = "./weights/TMVAClassification_BDT.weights.xml";
    Setup(xmlFile);
  }
  if (iTMVA)   OutFile += "TMVARank";
  if (RankMin) OutFile += "R"; 
  OutFile += ".root";
  fOut = TFile::Open(OutFile,"recreate");
  const Int_t nMcRecMult = 75; // 100;
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
  enum {NH = 4, NP = 4};
  const Name_t HCases[NH] = {
    {"Any", "Any vertex matched with MC == 1"},
    {"Good","The best rank vertex with MC == 1"},
    {"Bad", "The best rank vertex with MC != 1"},
    {"BadT","The best rank vertex with MC != 1 and bunch crossing == 0"}
  };
  const Name_t Plots[NP] = {
    {"Mult"    ,"the reconstructed (uncorrected) track multiplicity versus Reconstructable multiplicity"},
    {"MultQA"  ,"the reconstructed (corrected for QA) track multiplicity versus Reconstructable multiplicity"},
    {"McRecMul","Reconstructable multiplicity"},
    {"Rank"    ,"Rank"}
  };
  TH1D *McRecMulT = new TH1D("McRecMulT","Reconstructable multiplicity for Mc = 1", nMcRecMult, xMult.GetArray());
  TH1D *PrmVxMult = new TH1D("PrmVxMult","Total no. of reconstructed Vertices per  event",250,0,250.);
  /*          h   p  */
  TH1 *hists[NH][NP];  memset(hists, 0, sizeof(hists));
  for (Int_t h = 0; h < NH; h++) {
    for (Int_t p = 0; p < NP; p++) {
      TString Name(Plots[p].Name); Name += HCases[h].Name;
      TString Title(Plots[p].Title); Title += " for "; Title += HCases[h].Title; Title += " vertex";
      if      (p <  2)  hists[h][p] = new TH2D(Name,Title,nMcRecMult, xMult.GetArray(),nMcRecMult, xMult.GetArray());
      else if (p == 2)  hists[h][p] = new TH1D(Name,Title,nMcRecMult, xMult.GetArray());
      else if (p == 3)  hists[h][p] = new TH1D(Name,Title,2000,-1,1);
    }
  }
  TNtuple *Signal = 0, *Background = 0;
  Signal = new TNtuple("Signal","good vertex & global params info",TMVAdata::instance()->Names());
  Background = new TNtuple("Background","bad  vertex & global params info",TMVAdata::instance()->Names());
  Int_t bufsize= 64000;
  Int_t split  = -2;       // by default, split Event in sub branches
  if (split)  bufsize /= 4;
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  TTree::SetBranchStyle(branchStyle);
  TTree *tMcAll = new TTree("McAll"  , "All Mc Vertices");                                            tMcAll->Branch("Vx","Mc_t",&pMc);
  TTree *tMc    = new TTree("Mc"     , "Reconstractable Mc Vertex (> 1 tracks with 10 hits in Tpc)"); tMc   ->Branch("Vx","Mc_t",&pMc);
  TTree *tMatch = new TTree("Match", "Rc Vertex matched with only Mc one");			      tMatch->Branch("Vx","Match_t",&pMatch);
  TTree *tClone = new TTree("Clone",   "Mc Vertex matched with > 1 Rc Vertex (Clone)");		      tClone->Branch("Vx","Clone_t",&pClone);
  TTree *tLost  = new TTree("Lost",    "Mc Vertex not reconstucted");				      tLost ->Branch("Vx","Lost_t",&pLost);
  TTree *tGhost = new TTree("Ghost",   "Rc Vertex without Mc partner");				      tGhost->Branch("Vx","Ghost_t",&pGhost);
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
    "PrimaryTracks",
    "GlobalTracks",
    "StStMuMcVertex",
    "StStMuMcTrack",
    //    "CovPrimTrack",
    "CovGlobTrack",
    "StStMuMcVertex",
    "StStMuMcTrack",
    "KFTracks",
    "KFVertices",
    "StBTofHit",
    "StBTofHeader"
  }; 
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  Long64_t nevent = TMath::Min(Nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  if (nentries < 100) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);

  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    doPrint = _debugAsk || ev%100 == 0 || Debug() > 1;
    if (doPrint) 
      cout << "Read event #" << ev << "\tRun\t" << muEvent->runId() << "\tId: " << muEvent->eventId() << endl;
    PrimaryVertices   = mu->primaryVertices(); NoPrimaryVertices = PrimaryVertices->GetEntriesFast();
    PrmVxMult->Fill(NoPrimaryVertices);
    PrimaryTracks    = mu->array(muPrimary);   NoPrimaryTracks = PrimaryTracks->GetEntriesFast();
    GlobalTracks     = mu->array(muGlobal);    NoGlobalTracks = GlobalTracks->GetEntriesFast();
    CovPrimTrack     = mu->covPrimTrack();
    CovGlobTrack     = mu->covGlobTrack();
    KFVertices       = mu->KFVertices();       NoKFVertices = KFVertices->GetEntriesFast();
    KFTracks         = mu->KFTracks();         NoKFTracks = KFTracks->GetEntriesFast();
    MuMcVertices     = mu->mcArray(0);         NoMuMcVertices = MuMcVertices->GetEntriesFast();
    MuMcTracks       = mu->mcArray(1);         NoMuMcTracks = MuMcTracks->GetEntriesFast();
    if (doPrint) {
      cout << "PrimaryVertices " << NoPrimaryVertices;
      cout << "\tPrimaryTracks " << NoPrimaryTracks;
      cout << "\tGlobalTracks "  << NoGlobalTracks;
      cout << "\tCovPrimTrack "  << CovPrimTrack->GetEntriesFast();
      cout << "\tCovGlobTrack "  << CovGlobTrack->GetEntriesFast();
      cout << "\tKFVertices "    << NoKFVertices;
      cout << "\tKFTracks "      << NoKFTracks;
      cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices;
      cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks;
      cout << endl;
    }
    //    const Double_t field = muEvent->magneticField()*kilogauss;
    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    } else {
    Double_t VpdZ = -9999;
    StBTofHeader* BTofHeader = mu->btofHeader();
    if ( BTofHeader) {
      UInt_t NoWestHits = BTofHeader->numberOfVpdHits(west);
      UInt_t NoEastHits = BTofHeader->numberOfVpdHits(east);
      if ( NoWestHits > 0 &&  NoEastHits > 0) {
	VpdZ = BTofHeader->vpdVz();
      }
    }
    // Build maps --------------------------------------------------------------------------------
    multimap<StMuMcVertex *,StMuMcTrack *>      McVx2McTkR; // Reconstructable Mc Tracks
    map<StMuMcVertex *,StMuMcTrack *>           McVx2McParentTk; 
    map<Int_t,StMuMcTrack *>                    Id2McTk; // 
    map<Int_t,StMuMcVertex *>                   Id2McVx; // All Mc Vx, StMuMcVertex *McVx = Id2McVx[Id];
    map<Int_t,StMuMcVertex *>                   Id2McVxR;// Reconstructable, i.e. contains > 1 Reconstructable Mc Tracks
    map<Int_t,StMuPrimaryVertex*>               Id2RcVx;
    multimap<StMuPrimaryVertex*, StMuTrack *>   RcVx2RcTk;
    map<StMuPrimaryVertex*,StMuMcVertex *>      RcVx2McVx;
    multimap<StMuMcVertex *,StMuPrimaryVertex*> McVx2RcVx;
    vector<StMuPrimaryVertex *>                 RcVxs;  // All accepted RcVx
    vector<StMuPrimaryVertex *>                 RecoVx;  //  1 to 1 Mc to Rc match
    vector<StMuPrimaryVertex *>                 CloneVx; //  1 to many (>1) Mc to Rc match
    vector<StMuPrimaryVertex *>                 GhostVx; //  no Mc match
    vector<StMuMcVertex *>                      LostVx;  //  no Rc match
    map<Int_t,KFParticle*>                      IdVx2KFVx; // 
    map<KFParticle*,StMuPrimaryVertex*>         KFVx2RcVx;
    multimap<StMuPrimaryVertex*,KFParticle*>    RcVx2KFVx;
    typedef multimap<StMuPrimaryVertex*,KFParticle*>::iterator RcVx2KFVxIter;
    map<KFParticle*,StMuMcVertex *>             KFVx2McVx; 
    multimap<StMuMcVertex*,KFParticle*>         McVx2KFVx; 
    multimap<Int_t,StMuTrack *>                 IdMc2RcTk; // Reconstucted Track to IdTruth
    PrPPDH(McVx);
    // IdVx => McVx
    for (Int_t m = 0; m < NoMuMcVertices; m++) {
      StMuMcVertex *McVx = (StMuMcVertex *) MuMcVertices->UncheckedAt(m);
      if (! McVx) continue;
      PrPPD(*McVx);
      Int_t Id = McVx->Id();
      if (! Id) {
	cout << "Illegal Id:" << *McVx << " rejected." << endl;
	continue;
      }
      Id2McVx[Id] = McVx;
    }
    PrPP(Id2McVx.size());
    PrPPDH(IdMc2RcTk);
    // Id (Truth, IdMcTk) => gTrack
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (! Accept(gTrack)) continue;
      Int_t IdTruth = gTrack->idTruth();
      if (! IdTruth) continue;
      IdMc2RcTk.insert(pair<Int_t,StMuTrack *>(IdTruth,gTrack));
    }
    PrPP(IdMc2RcTk.size());
    PrPPDH(McTk);
    //  Id => McTk
    for (Int_t m = 0; m < NoMuMcTracks; m++) {
      StMuMcTrack *McTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m);
      if (! McTrack) continue;
      Int_t Id = McTrack->Id();
      if (! Id) {
	cout << "Illegal Id:" << *McTrack << " rejected." << endl;
	continue;
      }
      Id2McTk[Id] = McTrack;
    }
    // McEndVx => parent McTk
    for (auto x : Id2McVx) {
      Int_t IdVx = x.first; 
      StMuMcVertex *McVx = x.second;
      if (! IdVx || ! McVx) {
	cout << "Illegal Idx or McVx ==> rejected." << endl;
	continue;
      }
      Int_t IdParTk = McVx->IdParTrk();
      if (! IdParTk) continue;
      StMuMcTrack *McTrack = Id2McTk[IdParTk];
      PrPP2D(*McTrack,*McVx);
      McVx2McParentTk[McVx] = McTrack;
    }
    
    PrPP(Id2McTk.size());
    // McVx => McTkR
    for (auto x : Id2McTk) {
      Int_t Id = x.first;
      StMuMcTrack *McTrack = x.second;
      if (! Id || ! McTrack) {
	cout << "Illegal Mc Track Id or McTrack" << endl;
	continue;
      }
      Int_t IdVx = McTrack->IdVx();
      if (! IdVx) {
	cout << "Illegal IdVx:" << *McTrack << " rejected." << endl;
	continue;
      }
      Int_t n  = IdMc2RcTk.count(Id);
      if (! n) continue;
      //      if (McTrack->No_tpc_hit() < 10) continue;
      StMuMcVertex *mcVx = Id2McVx[IdVx];
      if (! mcVx) {
	cout << "Missing vertex of origin"; PrP(*McTrack);
	continue;
      }
      McVx2McTkR.insert(pair<StMuMcVertex *,StMuMcTrack *>(mcVx,McTrack));
    }
    PrPP(McVx2McParentTk.size());
    PrPP(McVx2McTkR.size());
    for (auto x : McVx2McTkR) {
      if (! x.first) continue;
      Int_t n = McVx2McTkR.count(x.first);
      if (n < 2) continue;
      Int_t Id = x.first->Id();
      Id2McVxR[Id] = x.first; 
    }
    PrPP(Id2McVxR.size());
    PrPPDH(RcVx);
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *RcVx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      //      if (! AcceptVX(RcVx)) continue;
      PrPP(*RcVx);
      Int_t Id = RcVx->id();
      RcVxs.push_back(RcVx); 
      Id2RcVx[Id] = RcVx;
      Int_t IdMc = RcVx->idTruth();
      if (! IdMc) {
	GhostVx.push_back(RcVx);
      }
      StMuMcVertex *McVx = Id2McVx[IdMc]; 
      if (McVx) {
	PrPP(*McVx);
	RcVx2McVx[RcVx] = McVx;
	McVx2RcVx.insert(make_pair(McVx,RcVx));
      }
    }
    PrPP(RcVxs.size());
    PrPP(McVx2RcVx.size()); PrPP(Id2RcVx.size());
    PrPP(GhostVx.size());

    PrPPDH(pTrack);
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
      if (! Accept(pTrack)) continue;
      Int_t IdVx = pTrack->vertexIndex();
      StMuPrimaryVertex *RcVx = Id2RcVx[IdVx];
      RcVx2RcTk.insert(make_pair(RcVx,pTrack));      
    }
    for (auto x: Id2McVxR) {
      if (! x.first) continue;
      StMuMcVertex *McVx = x.second;
      if (! McVx) continue;
      pair<
      multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator,
	multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator> ret = McVx2RcVx.equal_range(McVx);
      Int_t ncount = 0;
      StMuPrimaryVertex *RcVx = 0;
      for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	StMuPrimaryVertex *aRcVx = it->second;
	if (! aRcVx) continue;
	ncount++;
	if (! RcVx) {RcVx = aRcVx;}
	else if (RcVx->ranking() < aRcVx->ranking()) {RcVx = aRcVx;}
      }
      if (ncount == 0) {
	LostVx.push_back(McVx); 
      } else {
	for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	  StMuPrimaryVertex *aRcVx = it->second;
	  if (RcVx == aRcVx)  RecoVx.push_back(aRcVx);
	  else                CloneVx.push_back(aRcVx);
	}
      }
    }
    PrPP(LostVx.size());
    PrPP(RecoVx.size());
    PrPP(CloneVx.size());
#if 0
    PrPPDH(KFTrack);
    for (Int_t m = 0; m < NoKFTracks; m++) {
      KFParticle *particle = (KFParticle *) KFTracks->UncheckedAt(m);
      if (! particle) continue;
      PrPPD(*particle);
    }
#endif
    PrPPDH(KFVx);
    for (Int_t m = 0; m < NoKFVertices; m++) {
      KFParticle *KFVx = (KFParticle *) KFVertices->UncheckedAt(m);
      if (! KFVx) continue;
      PrPP(*KFVx);
#if 1
      if (doPrint) {
	cout << "NDaughters = " << KFVx->NDaughters() << endl;
	for (Int_t i = 0; i < KFVx->NDaughters(); i++) {
	  cout << "\t" << KFVx->DaughterIds()[i];
	  if (i%10 == 9) cout << endl;
	}
	cout << endl;
      }
#endif
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      Int_t IdParentID = KFVx->GetParentID();
      Int_t IdParentMcVx = KFVx->IdParentMcVx();
      IdVx2KFVx[IdVx] = KFVx;
      StMuPrimaryVertex* RcVx = Id2RcVx[IdVx];
      if (RcVx) {
	PrPP(*RcVx);
	KFVx2RcVx[KFVx] = RcVx;
	RcVx2KFVx.insert(pair<StMuPrimaryVertex*,KFParticle*>(RcVx,KFVx));
      }
      StMuMcVertex *McVx = RcVx2McVx[RcVx];
      if (McVx) {
	PrPP(*McVx);
	McVx2KFVx.insert(pair<StMuMcVertex *,KFParticle *>(McVx,KFVx));
      }
    }
    for (Int_t m = 0; m < NoKFTracks; m++) {
      KFParticle *KFVx = (KFParticle *) KFTracks->UncheckedAt(m);
      if (! KFVx) continue;
      if (! KFVx->NDaughters()) continue;
      PrPP(*KFVx);
#if 1
      if (doPrint) {
	cout << "NDaughters = " << KFVx->NDaughters() << endl;
	for (Int_t i = 0; i < KFVx->NDaughters(); i++) {
	  cout << "\t" << KFVx->DaughterIds()[i];
	  if (i%10 == 9) cout << endl;
	}
	cout << endl;
      }
#endif
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      Int_t IdParentID = KFVx->GetParentID();
      Int_t IdParentMcVx = KFVx->IdParentMcVx();
      IdVx2KFVx[IdVx] = KFVx;
      StMuPrimaryVertex* RcVx = Id2RcVx[IdVx];
      if (RcVx) {
	PrPP(*RcVx);
	KFVx2RcVx[KFVx] = RcVx;
	RcVx2KFVx.insert(pair<StMuPrimaryVertex*,KFParticle*>(RcVx,KFVx));
      }
      StMuMcVertex *McVx = RcVx2McVx[RcVx];
      if (McVx) {
	PrPP(*McVx);
	McVx2KFVx.insert(pair<StMuMcVertex *,KFParticle *>(McVx,KFVx));
      }
    }
    PrPP(IdVx2KFVx.size());
    PrPP(McVx2KFVx.size());
    // Done with maps --------------------------------------------------------------------------------
    // Primary vertices:
    StMuMcVertex *McVx1 = Id2McVx[1];
    Int_t NoMcTracksWithHitsAtMC1 = McVx2McTkR.count(McVx1); // @ primary vertex 
    McRecMulT->Fill(NoMcTracksWithHitsAtMC1);
    if (NoMcTracksWithHitsAtMC1 <= 0) continue; // No reconstructable Mc Vertex => skip event
    // Find the best Mc to Rc match (lMcBest)
    // Fill trees for signal (lMcBest, or an other primary interaction in the triggered buch crossing) and background
    Int_t lMcBest = -1; 
    Int_t MMult  = -1;
    UInt_t NoRcVertices = Id2RcVx.size();
    map<Int_t,PVgadgets_st> dataS;
    PVgadgets_st &aData = *(TMVAdata::instance()->GetArray());
    Int_t NoAcceptedRcVx = RcVxs.size();
    TArrayF Ranks(NoAcceptedRcVx);
    for (Int_t l = 0; l < NoAcceptedRcVx; l++) {
      StMuPrimaryVertex *RcVx = RcVxs[l];
      assert(RcVx);
      Int_t idd = RcVx->idTruth();
      StMuMcVertex *McVx = RcVx2McVx[RcVx];
      if (! McVx) continue;
      Ranks[l] = -1e10;
      UShort_t noTracks = RcVx->noTracks();
      if (Debug()) {
	cout << Form("%4i",l) << *RcVx << endl;
	if (idd) {
	  PrintMcVx(idd);
	  // loop over tracks which are not coming from the vertex and print these vertices
	  
	} 
      }
      if (idd == 1 && MMult < noTracks) {lMcBest = l; MMult = noTracks;} // 22
      Int_t NoMcTracksWithHits = 0;
      if (McVx) NoMcTracksWithHits = McVx2McTkR.count(McVx);
      FillData(*TMVAdata::instance(),RcVx,VpdZ,McVx, NoMcTracksWithHits);
      if (! TMVAdata::instance()->PPV() && ! aData.beam) continue;
      Ranks[l] = aData.Rank;
      Bool_t good = (l == lMcBest);
      if (! good) {// try pileup one
	good = (McVx && ! aData.timebucket);
      }
      aData.good = good;
      if (iTMVA) {
	Ranks[l] = StTMVARanking::instance()->Evaluate();
	aData.Rank = Ranks[l];
      }
      dataS[l] = aData;
    }
    Int_t lBest = TMath::LocMax(NoAcceptedRcVx, Ranks.GetArray());
    if (lBest < 0 || Ranks[lBest] < -1e9) continue; // Any reconstructed  vertices ?
    StMuPrimaryVertex *RcVx = RcVxs[lBest]; assert(RcVx);
    Double_t noTracks = RcVx->noTracks();
    Double_t noTracksQA = noTracks*RcVx->qaTruth()/100.;
    Int_t NoMcTracksWithHitsL = NoMcTracksWithHitsAtMC1;
    StMuMcVertex *McVx = RcVx2McVx[RcVx];
    if (McVx) NoMcTracksWithHitsL = McVx2McTkR.count(McVx);
    for (Int_t l = 0; l < NoAcceptedRcVx; l++) {
      StMuPrimaryVertex *RcVx = RcVxs[l];
      assert(RcVx);
      Int_t idd = RcVx->idTruth();
      StMuMcVertex *McVx = RcVx2McVx[RcVx];
      if (! McVx) continue;
      //    for (Int_t l = 0; l < NoRcVertices; l++) {
      //      if (Ranks[l] < -1e9) continue;
      dataS[l].l = l;
      dataS[l].lBest = lBest;
      dataS[l].lMcBest = lMcBest;
      aData = dataS[l];
      Bool_t good = aData.good; 
      if (good) {// good
	Signal->Fill((Float_t *) TMVAdata::instance()->GetArray());
      } else { // bad
	Background->Fill((Float_t *) TMVAdata::instance()->GetArray());
      }
      if (doPrint) {
	cout << Form("%3i Vx[%3i]", l, RcVx->id()) << *RcVx << Form(" rank %5.2f",Ranks[l]);
	if (McVx) {
	  cout << " " << *McVx;
	  Int_t NoMcTracksWithHitsatL = McVx2McTkR.count(McVx);
	  cout << Form(" No.McTkHit %4i", NoMcTracksWithHitsatL);
	  Int_t IdPar = McVx->IdParTrk();
	  if (IdPar > 0 && IdPar <= NoMuMcTracks) {
	    StMuMcTrack *McTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(IdPar-1);
	    if (McTrack) cout << " " << McTrack->GeName();
	  }
	}
	if (Ranks[l] != RcVx->ranking()) cout << " new Rank " << Form("%8.2f",Ranks[l]);
	if      (l == lMcBest)  {cout << "  === Mc Best === ";
	} else if (McVx && ! aData.timebucket) {
	  cout << "  === McPBest === ";
	} else if (l ==  lBest)   cout << "  ===    Best === ";
	else                    cout << "                 ";
	if (TMath::Abs(RcVx->position().z()-VpdZ) < 200) cout << Form("VpdZ %8.2f",RcVx->position().z()-VpdZ);
	cout << endl;
      }
    }
    aData = dataS[lBest];
    Int_t h = 1; 
    if (lBest == lMcBest) h = 1;
    else  {
      h = 2;
      if (aData.good) h = 3;
    }
    hists[0][3]->Fill(aData.Rank);
    hists[h][3]->Fill(aData.Rank);
    if (aData.Rank > RankMin) {
      hists[0][0]->Fill(NoMcTracksWithHitsL,noTracks);
      hists[0][1]->Fill(NoMcTracksWithHitsL,noTracksQA);
      hists[0][2]->Fill(NoMcTracksWithHitsAtMC1);
      hists[h][0]->Fill(NoMcTracksWithHitsL,noTracks);
      hists[h][1]->Fill(NoMcTracksWithHitsL,noTracksQA);
      hists[h][2]->Fill(NoMcTracksWithHitsAtMC1);
    }
    for (auto x : Id2McVx) {
      //      StMuMcVertex *McVx = x.first;
      if (! x.first) continue;
      StMuMcVertex *McVx = x.second;
      if (! McVx) continue;
      StMuMcTrack *McTrack = McVx2McParentTk[McVx];
      FillTrees(tMcAll,McVx,McTrack,0);
    }
    for (auto x : Id2McVxR) {
      //      StMuMcVertex *McVx = x.first;
      if (! x.first) continue;
      StMuMcVertex *McVx = x.second;
      if (! McVx) continue;
      StMuMcTrack *McTrack = McVx2McParentTk[McVx];
      FillTrees(tMc,McVx,McTrack,0);
    }
    PrPPDH(RecoVx);
    for (StMuPrimaryVertex *RcVx : RecoVx) {
      PrPP(*RcVx);
      StMuMcVertex *McVx = RcVx2McVx[RcVx];
      assert(McVx);
      PrintMcVx(McVx->Id());
      StMuMcTrack *McTrack = McVx2McParentTk[McVx];
#if 0
      PrPP(*McVx);
      if (McTrack) PrPP(*McTrack);
#endif      
      pair<RcVx2KFVxIter,RcVx2KFVxIter> ret = RcVx2KFVx.equal_range(RcVx);
      KFParticle* KFVx = 0;
      for (RcVx2KFVxIter it = ret.first; it != ret.second; ++it) {
	KFParticle* aKFVx = it->second;
	if (! KFVx) KFVx = aKFVx;
	else {
	  if ((aKFVx->GetNDF() > KFVx->GetNDF()) || 
	      (aKFVx->GetNDF() ==  KFVx->GetNDF() && aKFVx->GetChi2() < KFVx->GetChi2())) {
	    PrPP(*KFVx);
	    PrPP(*aKFVx);
	    KFVx = aKFVx;
	  }
	}
      }
      if (KFVx) {
	FillTrees(tMatch,McVx,McTrack,KFVx);
      }
    }
    PrPPDH(CloneVx);
    for (StMuPrimaryVertex *RcVx : CloneVx) {
      PrPP(*RcVx);
      StMuMcVertex *McVx = RcVx2McVx[RcVx];
      assert(McVx);
      StMuMcTrack *McTrack = McVx2McParentTk[McVx];
      PrintMcVx(McVx->Id());
#if 0
      PrPP(*McVx);
      if (McTrack) PrPP(*McTrack);
#endif
      KFParticle* KFVx = 0;
      pair<RcVx2KFVxIter,RcVx2KFVxIter> ret = RcVx2KFVx.equal_range(RcVx);
      for (RcVx2KFVxIter it = ret.first; it != ret.second; ++it) {
	KFParticle* aKFVx = it->second;
	if (! KFVx) KFVx = aKFVx;
	else {
	  if ((aKFVx->GetNDF() > KFVx->GetNDF()) || 
	      (aKFVx->GetNDF() ==  KFVx->GetNDF() && aKFVx->GetChi2() < KFVx->GetChi2())) {
	    PrPP(*KFVx);
	    PrPP(*aKFVx);
	    KFVx = aKFVx;
	  }
	}
      }
      if (KFVx) {
	FillTrees(tClone,McVx,McTrack,KFVx);
      }
    }
    PrPPDH(Lost);
    for (StMuMcVertex *McVx : LostVx) {
      StMuMcTrack *McTrack = McVx2McParentTk[McVx];
      PrintMcVx(McVx->Id());
#if 0
      PrPP(*McVx);
      if (McTrack) PrPP(*McTrack);
#endif
      FillTrees(tLost,McVx,McTrack,0);
    }
    PrPPDH(Ghost);
    for (StMuPrimaryVertex *RcVx : GhostVx) {
     PrPP(*RcVx); 
      KFParticle* KFVx = 0;
      pair<RcVx2KFVxIter,RcVx2KFVxIter> ret = RcVx2KFVx.equal_range(RcVx);
      for (RcVx2KFVxIter it = ret.first; it != ret.second; ++it) {
	KFParticle* aKFVx = it->second;
	if (! KFVx) KFVx = aKFVx;
	else {
	  if ((aKFVx->GetNDF() > KFVx->GetNDF()) || 
	      (aKFVx->GetNDF() ==  KFVx->GetNDF() && aKFVx->GetChi2() < KFVx->GetChi2())) {
	    PrPP(*KFVx);
	    PrPP(*aKFVx);
	    KFVx = aKFVx;
	  }
	}
      }
      if (KFVx) {
	FillTrees(tGhost,0,0,KFVx);
      }
    }
    if (! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
  }
  fOut->Write();
  if(! iTMVA) {
    Setup();
    StTMVARanking::TMVAClassification(TMVAMethod,Signal,Background);
    TMVAdata::instance()->Print();
  }
}
/* 
    Set axis label to GEANT particle names
*/
//________________________________________________________________________________
void SetGePidLabels(TAxis *x) {
  if (! x) return;
  const Char_t *names[51] = {"",//           1                  2                 3                  4                5 
			     "#gamma"         ,           "e^{+}",          "e^{-}",             "#nu",       "#mu^{+}", //  5 			
			     "#mu^{-}"        ,         "#pi^{0}",        "#pi^{+}",          "#pi^{-}",    "K^{0}_{L}", // 10 			
			     "K^{+}"          ,           "K^{-}",               "n",               "p",      "#bar{p}", // 15			
			     "K^{0}_{S}"      ,            "#eta",         "#Lambda",      "#Sigma^{+}",   "#Sigma^{0}", // 20		
			     "#Sigma^{-}"     ,         "#Xi^{0}",         "#Xi^{-}",      "#Omega^{-}",      "#bar{n}", // 25			
			     "#bar{#Lambda}"  ,"#bar{#Sigma}^{-}","#bar{#Sigma}^{0}","#bar{#Sigma}^{+}","#bar{#Xi}^{0}", // 30
			     "#bar{#Xi}^{+}"  ,"#bar{#Omega}^{+}",        "#tau^{+}",        "#tau^{-}",        "D^{+}", // 35              
			     "D^{-}"          ,           "D^{0}",     "#bar{D}^{0}",       "D_{s}^{+}",    "D_{s}^{-}", // 40
			     "#Lambda_{c}^{+}",           "W^{+}",           "W^{-}",           "Z^{0}",            "d", // 45
			     "t"              ,          "#alpha",               "g",          "He^{3}",     "#hat{C}"}; // 50
  Int_t n = x->GetNbins();
  for (Int_t i = 0; i < n; i++) {
    Int_t  j = TMath::Nint(x->GetBinCenter(i+1));
    if (j >= 0 && j <= 50) {
      x->SetBinLabel(i+1,names[j]);
    }
  }
}

//________________________________________________________________________________
void DrawMcVsRc() {
  if (fOut) fOut->cd();
  TTree *tMcAll = (TTree *) gDirectory->Get("McAll"); tMcAll->GetBranch("Vx")->SetAddress(&pMc);
  TTree *tMc    = (TTree *) gDirectory->Get("Mc")   ; tMc   ->GetBranch("Vx")->SetAddress(&pMc);
  TTree *tMatch = (TTree *) gDirectory->Get("Match"); tMatch->GetBranch("Vx")->SetAddress(&pMatch);
  TTree *tClone = (TTree *) gDirectory->Get("Clone"); tClone->GetBranch("Vx")->SetAddress(&pClone);
  TTree *tLost  = (TTree *) gDirectory->Get("Lost") ; tLost ->GetBranch("Vx")->SetAddress(&pLost);
  TTree *tGhost = (TTree *) gDirectory->Get("Ghost"); tGhost->GetBranch("Vx")->SetAddress(&pGhost);
  if (! tMcAll || ! tMc || ! tMatch) return;
  gStyle->SetLineWidth(2);
  tMcAll->Draw("Mc.Tk.mGePid>>GePidMcAll(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)","");
  TH1F *GePidMcAll = (TH1F *) gDirectory->Get("GePidMcAll");
  GePidMcAll->SetTitle("Vertex: Mc versus Rc, pythia pp 510 GeV W-sample");
  GePidMcAll->SetXTitle("Parent Track type");
  GePidMcAll->SetStats(0);
  SetGePidLabels(GePidMcAll->GetXaxis());
  GePidMcAll->SetLineWidth(4);
  GePidMcAll->Draw();
  TLegend *l = new TLegend(0.7,0.5,0.9,0.9);
  l->AddEntry(GePidMcAll,"All Mc Vx");
  l->Draw();
  //
  tMc->SetLineColor(2);
  tMc->Draw("Mc.Tk.mGePid>>GePidMc(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)","same");
  TH1F *GePidMc = (TH1F *) gDirectory->Get("GePidMc");
  GePidMc->SetLineWidth(4);
  l->AddEntry(GePidMc,"Mc Vx with > 1 RC Tracks");
  //
  tMatch->SetLineColor(5);
  tMatch->Draw("Mc.Tk.mGePid>>GePidMatch(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)","same");
  TH1F *GePidMatch = (TH1F *) gDirectory->Get("GePidMatch");
  GePidMatch->SetLineWidth(4);
  l->AddEntry(GePidMatch,"Rc Vx");
  tMatch->SetLineColor(4);
  tMatch->Draw("Mc.Tk.mGePid>>GePidMatchQ(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)&&V.fQuality>90","same");
  TH1F *GePidMatchQ = (TH1F *) gDirectory->Get("GePidMatchQ");
  GePidMatchQ->SetLineWidth(4);
  l->AddEntry(GePidMatchQ,"Rc Vx with Q > 90");
  for (Int_t NDF = 2; NDF <= 4; NDF += 2) {
    tMatch->SetLineColor(4+NDF);
    tMatch->Draw(Form("Mc.Tk.mGePid>>GePidMatch%i(50,-0.5,49.5)",NDF),
		 Form("!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)&&V.fQuality>90&&V.fNDF==%i",NDF),"same");
    TH1F *GePidMatch1 = (TH1F *) gDirectory->Get(Form("GePidMatch%i",NDF));
    GePidMatch1->SetLineWidth(4);
    l->AddEntry(GePidMatch1,Form("Rc Vx with Q > 90, NDF==%i",NDF));
  }
}  

/*
McAll->Draw("Mc.Tk.mGePid>>GePidMcAll(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)","")
.L SetGePidLabels.C+
SetGePidLabels(GePidMcAll->GetXaxis())

Mc->SetLineColor(2)
TLegend *l = new TLegend(0.7,0.5,0.9,0.9)
l->AddEntry(GePidMcAll,"All Mc Vx")
l->Draw()
Mc->SetLineColor(2)
Mc->Draw("Mc.Tk.mGePid>>GePidMc(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)","same")
l->AddEntry(GePidMc,"Mc Vx with > 1 RC Tracks")
GePidMcAll->SetTitle("Vertex Mc versus Rc")
Match->SetLineColor(3)
Match->Draw("Mc.Tk.mGePid>>GePidMatch(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)","same")
l->AddEntry(GePidMatch,"Rc Vx")
Match->Draw("Mc.Tk.mGePid>>GePidMatchQ(50,-0.5,49.5)","!(Mc.Vx.mId!=1&&Mc.Tk.mGePid==0)&&V.fQuality>90","same")
l->AddEntry(GePidMatchQ,"Rc Vx with Q > 90")



TH1F *frame = new TH1F(*GePidMcAll)
frame->SetName("frame");
frame->SetTitle("Efficiency wrt McR")
frame->Draw()
c1->SetLogy(0)
frame->Reset()
frame->SetMaximum(1.1)
TEfficiency *ef = new TEfficiency(*GePidMatch,*GePidMc)
ef->Draw("same")
TEfficiency *efq = new TEfficiency(*GePidMatchQ,*GePidMc)
efq->SetMarkerColor(2)
efq->Draw("same");
TLegend *ll = new TLegend(0.6,0.7,0.9,0.9)
ll->AddEntry(ef,"Rc Vx")
ll->Draw()
ll->AddEntry(efq,"Rc Vx with Q > 90")



*/
