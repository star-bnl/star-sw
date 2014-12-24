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
#include "TEfficiency.h"
#include "TChain.h"
#include "TString.h"
#include "TStyle.h"
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
#include "StTMVARank/TMVAdata.h"
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
#include "StTMVARank/TMVAClassification.h"
#include "StTMVARank/TMVAClassificationApplication.h"
const Char_t *TMVAMethod = "BDT";
TString TMVAMethodE(Form("%s method",TMVAMethod)); 
Bool_t iPPV = kFALSE;
Bool_t iPileUp = kFALSE;
Int_t iYear = -1;
TH2F *ppvRankC = 0;
using namespace std;
//________________________________________________________________________________
static Int_t _debug = 0;
void SetDebug(Int_t k) {_debug = k;}
Int_t Debug() {return _debug;}
//________________________________________________________________________________
void Setup() {
  TString CDir(gSystem->pwd());
  if (CDir.Contains("pileup")) {iPileUp = kTRUE; cout << "iPileUp is set" << endl;}
  if (CDir.Contains("PPV")) {
    TMVAdata::instance()->SetPPV();
    iPPV = kTRUE; cout << "iPPV is set" << endl;
    ppvRankC = new TH2F("ppvRankC","PPV Rank  conversion new versus old",2000,-2e6,2e6,2000,-1,1);
  }
  if (CDir.Contains("2011")) iYear = 2011; 
  if (CDir.Contains("2012")) iYear = 2012; 
  cout << "iYear set to " << iYear << endl;
  TMVAdata::instance()->Init();
}
//________________________________________________________________________________
void Classification(const Char_t *method = TMVAMethod, 
		    const Char_t  *signalT = "Signal", 
		    const Char_t *backgroundT = "Background") {
  Setup();
  TTree *signal     = (TTree*)gDirectory->Get(signalT);
  if (! signal) { cout << "No signal TTree" << endl; return;}
  TTree *background = (TTree*)gDirectory->Get(backgroundT);
  if (! background) { cout << "No background TTree" << endl; return;}
  std::map<std::string,int> Use;
  TMVA::Factory *factory = TMVAClassification(method,Use);
  TTableDescriptor *ds = TMVAdata::instance()->GetTableDesc();
  tableDescriptor_st *s = ds->GetTable();
  for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
    TString aName(s->fColumnName);
    if (! TMVAdata::instance()->AcceptVar(aName)) {
      factory->AddSpectator(aName, 'F');
    } else {
      // check RMS
      background->Draw(Form("%s >> temp",aName.Data()));
      TH1 *temp = (TH1 *) gDirectory->Get("temp");
      if (temp && temp->GetRMS() > 1e-3 * temp->GetMean()) {
	cout << "Add variable: " << aName << endl;
	factory->AddVariable(aName, 'F');
      } else {
	TMVAdata::instance()->fAcceptMap[aName] = kFALSE;
	factory->AddSpectator(aName, 'F');
      }
    }
  }
   // global event weights per tree (see below for setting event-wise weights)
  Double_t signalWeight     = 1.0;
  Double_t backgroundWeight = 1.0;
   
  // You can add an arbitrary number of signal or background trees
  factory->AddSignalTree    ( signal,     signalWeight     );
  factory->AddBackgroundTree( background, backgroundWeight );
  MakeClassification(factory, Use);
}
//________________________________________________________________________________
TMVA::Reader *ClassificationApplication(const Char_t *method = "") {
  
  std::map<std::string,int> Use;
  TMVA::Reader *reader = TMVAClassificationApplication(method, Use);
  TTableDescriptor *ds = TMVAdata::instance()->GetTableDesc();
  tableDescriptor_st *s = ds->GetTable();
  Float_t *dataArray = (Float_t *) TMVAdata::instance()->GetArray();
  for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
    TString aName(s->fColumnName);
    if (! TMVAdata::instance()->AcceptVar(aName)) {
      reader->AddSpectator(aName,  dataArray+i);
    } else {
      reader->AddVariable(aName, dataArray+i);
    }
    i++;
  }
  BookTMVAMethod(reader,Use);
  return reader;
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
  if (iPPV) {
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
void PrintMcVx(Int_t idVx = 1, TClonesArray *MuMcVertices = 0, TClonesArray *MuMcTracks = 0) {
  if (! MuMcVertices) return;
  if (idVx < 0 || idVx > MuMcVertices->GetEntriesFast()) return;
  StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idVx-1);	
  if (! mcVertex) return;
  cout << " " << *mcVertex;
  if (MuMcTracks) {
    Int_t iMcTk = mcVertex->IdParTrk();
    if (iMcTk > 0 && iMcTk <= MuMcTracks->GetEntriesFast()) {
      StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(iMcTk-1);
      if (mcTrack) cout << "\t" << mcTrack->GeName();
    }
  }
  cout << endl;
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
  if (! iPPV && !  Vtx->isBeamConstrained()) return kFALSE; //20
#endif
  //21  if (  Vtx->position().perp() > 3.0) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
void ForceAnimate(unsigned int times=0, int msecDelay=0) {
  unsigned int  counter = times;
  while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
}
//________________________________________________________________________________
void MuMcPrV(Bool_t iTMVA = kFALSE, Float_t RankMin = 0, Long64_t Nevent = 999999, 
	     const char* file="*.MuDst.root",
	     const  char* outFile="MuMcPrV36") { 
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
  // 35 use table
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
  TMVA::Reader *reader = 0;
  if (iTMVA)    reader = ClassificationApplication(TMVAMethod);
#if 0
  // Classifier Cut Efficiency
  if (iYear == 2012) {
    if (! iPPV) {  // KVF
      if (! iPileUp ) RankMin = -0.189; // W +0.091;
      else            RankMin = -0.172; // W -0.082;
    } else {       // PPV
      if (! iPileUp ) RankMin = -0.288; // W -0.019;
      else            RankMin = -0.278;
    }
  }
#endif
  if (iYear < 0) {
    Setup();
  }
  if (iTMVA)   OutFile += "TMVARank";
  if (RankMin) OutFile += "R"; 
  OutFile += ".root";
  TFile *fOut = TFile::Open(OutFile,"recreate");
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
    "StStMuMcVertex","StStMuMcTrack",
    "StBTofHit","StBTofHeader"
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
    if (_debugAsk || ev%100 == 0) 
      cout << "Read event #" << ev << "\tRun\t" << muEvent->runId() << "\tId: " << muEvent->eventId() << endl;
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (_debugAsk || ev%100 == 0) cout << "\tPrimaryVertices " << NoPrimaryVertices;
    PrmVxMult->Fill(NoPrimaryVertices);
#if 1
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      if (_debugAsk || ev%100 == 0) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (_debugAsk || ev%100 == 0) {cout << "\tGlobalTracks " << NoGlobalTracks;}
#endif
    TClonesArray *MuMcVertices   = mu->mcArray(0); 
    Int_t NoMuMcVertices = MuMcVertices->GetEntriesFast(); if (_debugAsk || ev%100 == 0) cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices;
    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    Int_t NoMuMcTracks = MuMcTracks->GetEntriesFast(); if (_debugAsk || ev%100 == 0) cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks;
    //    const Double_t field = muEvent->magneticField()*kilogauss;
    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    }
    // Count no. track at a vertex with TPC reconstructable tracks.
    multimap<Int_t,Int_t> Mc2McHitTracks;
    map<Int_t,PVgadgets_st> dataS;
    for (Int_t m = 0; m < NoMuMcTracks; m++) {
      StMuMcTrack *McTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(m);
      if (McTrack->No_tpc_hit() < 15) continue;
      Mc2McHitTracks.insert(pair<Int_t,Int_t>(McTrack->IdVx(),McTrack->Id()));
    }
    Int_t NoMcTracksWithHitsAtMC1 = Mc2McHitTracks.count(1); // @ primary vertex 
    McRecMulT->Fill(NoMcTracksWithHitsAtMC1);
    if (NoMcTracksWithHitsAtMC1 <= 0) continue;
    // =============  Build map between  Rc and Mc vertices
    if (_debugAsk || ev%100 == 0) cout << endl;
    TArrayF Ranks(NoPrimaryVertices);
    //Mc: any vertex with MC==1 and highest reconstrated multiplicity. 
    //Rc: any vertex closest to vpdVz
    Int_t lMcBest = -1; 
    Int_t MMult  = -1;
    Double_t VpdZ = -9999;
    StBTofHeader* BTofHeader = mu->btofHeader();
    if ( BTofHeader) {
      UInt_t NoWestHits = BTofHeader->numberOfVpdHits(west);
      UInt_t NoEastHits = BTofHeader->numberOfVpdHits(east);
      if ( NoWestHits > 0 &&  NoEastHits > 0) {
	VpdZ = BTofHeader->vpdVz();
      }
    }
    // Find the best Mc to Rc match (lMcBest)
    map<StMuPrimaryVertex *,StMuMcVertex *> Mc2RcVertices;
    PVgadgets_st &aData = *(TMVAdata::instance()->GetArray());
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (! AcceptVX(Vtx)) continue;
      //      Vtx->Print();
      UShort_t noTracks = Vtx->noTracks();
      Int_t idd = Vtx->idTruth();
      // Check Mc
      if (idd <= 0 || idd > NoMuMcVertices) continue;
      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idd-1);
      if (mcVertex->Id() != idd) {
	cout << "Mismatched idTruth " << idd << " and mcVertex Id " <<  mcVertex->Id() 
	     << " The vertex is ignored" <<  endl;
	continue;
      }
      Mc2RcVertices[Vtx] = mcVertex;
      if (Debug()) {
	cout << Form("%4i",l) << *Vtx;
	if (idd) {
	  PrintMcVx(idd,MuMcVertices,MuMcTracks);
	  // Print info for tracks coming from wrong verticies
	  for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	    StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	    if (pTrack->vertexIndex() != l) continue;
	    if (pTrack->idTruth() < 0 || pTrack->idTruth() > NoMuMcTracks) {
	      cout << "Illegal idTruth " << pTrack->idTruth() << " The track is ignored" << endl;
	      continue;
	    }
	    StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(pTrack->idTruth()-1);
	    if (mcTrack->Id() != pTrack->idTruth()) {
	      cout << "Mismatched idTruth " << pTrack->idTruth() << " and mcTrack Id " <<  mcTrack->Id() 
		   << " The track is ignored" <<  endl;
	    }
	    Int_t iddW = mcTrack->IdVx();
	    if (iddW <= 0 || iddW > NoMuMcVertices) continue;
	    if (iddW == idd) continue;
	    cout << "Wrong vertex\t"; PrintMcVx(iddW,MuMcVertices,MuMcTracks);
	  }
	} else {cout << endl;}
      }
      if (idd == 1 && MMult < noTracks) {lMcBest = l; MMult = noTracks;} // 22
    }
    // Fill trees for signal (lMcBest, or an other primary interaction in the triggered buch crossing) and background
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      Ranks[l] = -1e10;
      if (! AcceptVX(Vtx)) continue;
      //      Vtx->Print();
      UShort_t noTracks = Vtx->noTracks();
      Int_t idd = Vtx->idTruth();
      // Check Mc
      StMuMcVertex *mcVertex = 0;
      if (idd > 0 && idd <= NoMuMcVertices) {
	mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idd-1);
	if (mcVertex->Id() != idd) {
	  cout << "Mismatched idTruth " << idd << " and mcVertex Id " <<  mcVertex->Id() 
	       << " The vertex is ignored" <<  endl;
	  continue;
	}
      }
      Mc2RcVertices[Vtx] = mcVertex;
      if (Debug()) {
	cout << Form("%4i",l) << *Vtx;
	if (idd) {
	  PrintMcVx(idd,MuMcVertices,MuMcTracks);
	  // loop over tracks which are not coming from the vertex and print these vertices
	  
	} else {cout << endl;}
      }
      if (idd == 1 && MMult < noTracks) {lMcBest = l; MMult = noTracks;} // 22
      Int_t NoMcTracksWithHits = 0;
      if (mcVertex) NoMcTracksWithHits = Mc2McHitTracks.count(mcVertex->Id());
      FillData(*TMVAdata::instance(),Vtx,VpdZ,mcVertex, NoMcTracksWithHits);
      
      Ranks[l] = aData.Rank;
      Bool_t good = (l == lMcBest);
      if (! good) {// try pileup one
	good = (mcVertex && ! aData.timebucket);
      }
      aData.good = good;
      if (reader) {
	Ranks[l] = reader->EvaluateMVA(TMVAMethodE);
	aData.Rank = Ranks[l];
      }
      dataS[l] = aData;
    }
    Int_t lBest = TMath::LocMax(NoPrimaryVertices, Ranks.GetArray());
    if (lBest < 0 || Ranks[lBest] < -1e9) continue; // Any reconstructed  vertices ?
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      if (Ranks[l] < -1e9) continue;
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
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      StMuMcVertex *mcVertex = Mc2RcVertices[Vtx];
      if (_debugAsk || ev%100 == 0) {
	cout << Form("Vx[%3i]", l) << *Vtx;
	if (mcVertex) {
	  cout << " " << *mcVertex;
	  Int_t NoMcTracksWithHitsatL = Mc2McHitTracks.count(Vtx->idTruth());
	  cout << Form(" No.McTkHit %4i", NoMcTracksWithHitsatL);
	  Int_t IdPar = mcVertex->IdParTrk();
	  if (IdPar > 0 && IdPar <= NoMuMcTracks) {
	    StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(IdPar-1);
	    if (mcTrack && (_debugAsk || ev%100 == 0)) cout << " " << mcTrack->GeName();
	  }
	}
	if (Ranks[l] != Vtx->ranking()) cout << " new Rank " << Form("%8.2f",Ranks[l]);
	if      (l == lMcBest)  {cout << "  === Mc Best === ";
	} else if (mcVertex && ! aData.timebucket) {
	  cout << "  === McPBest === ";
	} else if (l ==  lBest)   cout << "  ===    Best === ";
	else                    cout << "                 ";
	if (TMath::Abs(Vtx->position().z()-VpdZ) < 200) cout << Form("VpdZ %8.2f",Vtx->position().z()-VpdZ);
	cout << endl;
      }
    }
    Int_t l = lBest;
    StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
    Double_t noTracks = Vtx->noTracks();
    aData = dataS[l];
    Double_t noTracksQA = noTracks*Vtx->qaTruth()/100.;
    Int_t NoMcTracksWithHitsL = NoMcTracksWithHitsAtMC1;
    if (Vtx->idTruth()) NoMcTracksWithHitsL = Mc2McHitTracks.count(Vtx->idTruth());
    Int_t h = 1; 
    if (l == lMcBest) h = 1;
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
    if (! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
  }
  fOut->Write();
#if 1
  if(! reader) {
    Classification(TMVAMethod,"Signal","Background");
    TMVAdata::instance()->Print();
  }
#endif
}
