//#define DEBUG
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class TDirIter;
class TTreeIter;
#endif
TFile *fOut = 0;
//________________________________________________________________________________
void muDst(const Char_t *files,const Char_t *Out="Output.root") {
  cout << "files " << files << "\t output " << Out << endl;
  if (Out) {
    fOut = new TFile(Out,"RECREATE");
  }
  TH2D *prVSgl = new TH2D("prVSgl","no. of primary vs global",1000,0,5000,1000,0,5000);
  TH2D *prVSgl15 = new TH2D("prVSgl15","no. of primary vs global with no. Tpc hits >= 15",
			    1000,0,5000,1000,0,5000);
  TH1D *nPrim = new TH1D("nPrim","no .of Primary verteces",50,0,50);
  TH2D *vtXZ = new TH2D("vtXZ","x vs Z at vetex", 100, -50, 50, 100, -.5, 0.5);
  TH2D *vtYZ = new TH2D("vtYZ","y vs Z at vetex", 100, -50, 50, 100, -.5, 0.5);
  TH2D *vtXY = new TH2D("vtXY","y vs x at vetex", 100,  -.5,.5, 100, -.5, 0.5);

  TH2D *pTfitP = new TH2D("pTfitP","NHitsG versus log10(pT), positive |eta| <1", 100, 0, 10., 50, -2, 3);
  TH2D *pTfitN = new TH2D("pTfitN","NHitsG versus log10(pT), negative |eta| <1", 100, 0, 10., 50, -2, 3);
  TH2D *SvtHitG = new TH2D("SvtHitG","No.of Svt hits vesus no. of possible",10,0,10,10,0,10);
  TH2D *TpcHitG = new TH2D("TpcHitG","No.of Tpc hits vesus no. of possible",50,0,50,50,0,50);
  TH2D *SvtHit = new TH2D("SvtHit","No.of Svt hits vesus no. of possible",10,0,10,10,0,10);
  TH2D *TpcHit = new TH2D("TpcHit","No.of Tpc hits vesus no. of possible",50,0,50,50,0,50);
  TH2D *ratZG  = new TH2D("ratG","no.primary/no.glob versus Z",100,-50.,50.,100,0.,1.);
  TH2D *ratZG15  = new TH2D("ratG15","no.primary/no.glob versus Z with No.TPC hits >=15",
			    100,-50.,50.,100,0.,1.);
  TH1D *dcaG   = new TH1D("dcaG","3D dca",100,0,20.);
  TH1D *dcaG15   = new TH1D("dcaG15","3D dca NoTpcHits > 15",100,0,20.);
  TH1D *chisqXYG   = new TH1D("chisqXYG","ChisqXY",100,0,100.);
  TH1D *chisqXYG15   = new TH1D("chisqXYG15","ChisqXY NoTpcHits > 15",100,0,100.);
  TH1D *chiSqZG   = new TH1D("chiSqZG","ChiSqZ",100,0,100.);
  TH1D *chiSqZG15   = new TH1D("chiSqZG15","ChiSqZ NoTpcHits > 15",100,0,100.);

  TH1D *dca   = new TH1D("dca","3D dca",100,0,20.);
  TH1D *dca15   = new TH1D("dca15","3D dca NoTpcHits > 15",100,0,20.);
  TH1D *chisqXY   = new TH1D("chisqXY","ChisqXY",100,0,100.);
  TH1D *chisqXY15   = new TH1D("chisqXY15","ChisqXY NoTpcHits > 15",100,0,100.);
  TH1D *chiSqZ   = new TH1D("chiSqZ","ChiSqZ",100,0,100.);
  TH1D *chiSqZ15   = new TH1D("chiSqZ15","ChiSqZ NoTpcHits > 15",100,0,100.);

  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  //  cout << files << "\twith " << NFiles << " files" << endl; 
  // init of user variables
  const Int_t    &NoPrimaryVertices    = iter("PrimaryVertices");
  const UShort_t*&NTracksUsed      = iter("PrimaryVertices.mNTracksUsed"); 
  const Float_t *&vertexX        = iter("Event.mPrimaryVertexX");
  const Float_t *&vertexY        = iter("Event.mPrimaryVertexY");
  const Float_t *&vertexZ        = iter("Event.mPrimaryVertexZ");

  const Int_t    &NoTracksG      = iter("GlobalTracks");
  //  const UChar_t *&NHitsG         = iter("GlobalTracks.mNHits");   
  const UChar_t *&NHitsFitG      = iter("GlobalTracks.mNHitsFit");   
  //  const UChar_t *&NHitsPossG     = iter("GlobalTracks.mNHitsPoss");   
  const UChar_t *&NHitsPossInnerG= iter("GlobalTracks.mNHitsPossInner");   
  const UChar_t *&NHitsFitInnerG = iter("GlobalTracks.mNHitsFitInner");   
  const UChar_t *&NHitsPossTpcG  = iter("GlobalTracks.mNHitsPossTpc");   
  const UChar_t *&NHitsFitTpcG   = iter("GlobalTracks.mNHitsFitTpc");   
  const Float_t *&pTG            = iter("GlobalTracks.mPt");   
  const Float_t *&EtaG           = iter("GlobalTracks.mEta");   
  const Short_t *&QG             = iter("GlobalTracks.mHelix.mQ");   
  const Float_t *&ChiSqXYG       = iter("GlobalTracks.mChiSqXY");
  const Float_t *&ChiSqZG        = iter("GlobalTracks.mChiSqZ");
  const Float_t *&dcaXG          = iter("GlobalTracks.mDCA.mX1");
  const Float_t *&dcaYG          = iter("GlobalTracks.mDCA.mX2");
  const Float_t *&dcaZG          = iter("GlobalTracks.mDCA.mX3");

  const Int_t    &NoTracks       = iter("PrimaryTracks");
  //  const UChar_t *&NHits          = iter("PrimaryTracks.mNHits");   
  //  const UChar_t *&NHitsFit       = iter("PrimaryTracks.mNHitsFit");   
  //  const UChar_t *&NHitsPoss      = iter("PrimaryTracks.mNHitsPoss");   
  const UChar_t *&NHitsPossInner = iter("PrimaryTracks.mNHitsPossInner");   
  const UChar_t *&NHitsFitInner  = iter("PrimaryTracks.mNHitsFitInner");   
  const UChar_t *&NHitsPossTpc   = iter("PrimaryTracks.mNHitsPossTpc");   
  const UChar_t *&NHitsFitTpc    = iter("PrimaryTracks.mNHitsFitTpc");   
  const Float_t *&pT             = iter("PrimaryTracks.mPt");   
  const Float_t *&Eta            = iter("PrimaryTracks.mEta");   
  const Short_t *&Q              = iter("PrimaryTracks.mHelix.mQ");   
  const Float_t *&ChiSqXY        = iter("PrimaryTracks.mChiSqXY");
  const Float_t *&ChiSqZ         = iter("PrimaryTracks.mChiSqZ");
  const Float_t *&dcaX           = iter("PrimaryTracks.mDCA.mX1");
  const Float_t *&dcaY           = iter("PrimaryTracks.mDCA.mX2");
  const Float_t *&dcaZ           = iter("PrimaryTracks.mDCA.mX3");
  //         Now iterations
  while (iter.Next()) {
    nPrim->Fill(NoPrimaryVertices);
    for (Int_t ipr = 0; ipr < NoPrimaryVertices; ipr++) {
      if (NTracksUsed[ipr] > 0) {
	vtXZ->Fill(vertexZ[ipr],vertexX[ipr]);
	vtYZ->Fill(vertexZ[ipr],vertexY[ipr]);
	vtXY->Fill(vertexX[ipr],vertexY[ipr]);
      }
    }
    Int_t NoTracksG15 = 0;
    Int_t NoTracks15 = 0;
    Int_t NtpcG = 0;
    Int_t Ntpc = 0;
    for (Int_t k=0;k<NoTracksG;k++) {
      if (! NHitsFitTpcG[k]) continue;
      NtpcG++;
      if (TMath::Abs(EtaG[k]) > 1) continue;
      if (pTG[k]    > 10.|| ! QG[k] ) continue;
      if (Q[k] > 0) pTfitP->Fill(TMath::Log10(pTG[k]),NHitsFitG[k]);
      else          pTfitN->Fill(TMath::Log10(pTG[k]),NHitsFitG[k]);
      SvtHitG->Fill(NHitsPossInnerG[k],NHitsFitInnerG[k]);
      TpcHitG->Fill(NHitsPossTpcG[k],NHitsFitTpcG[k]);
      dcaG->Fill(TMath::Sqrt(dcaXG[k]*dcaXG[k]+dcaYG[k]*dcaYG[k]+dcaZG[k]*dcaZG[k]));
      chisqXYG->Fill(ChiSqXYG[k]);
      chiSqZG->Fill(ChiSqZG[k]);	
      if (NHitsFitTpcG[k] < 15) continue;
      NoTracksG15++;
      dcaG15->Fill(TMath::Sqrt(dcaXG[k]*dcaXG[k]+dcaYG[k]*dcaYG[k]+dcaZG[k]*dcaZG[k]));
      chisqXYG15->Fill(ChiSqXYG[k]);
      chiSqZG15->Fill(ChiSqZG[k]);	
    }
    for (Int_t k=0;k<NoTracks;k++) {
      if (! NHitsFitTpc[k]) continue;
      Ntpc++;
      if (TMath::Abs(Eta[k]) > 1) continue;
      if (pT[k]    > 10.|| ! Q[k] ) continue;
      SvtHit->Fill(NHitsPossInner[k],NHitsFitInner[k]);
      TpcHit->Fill(NHitsPossTpc[k],NHitsFitTpc[k]);
      dca->Fill(TMath::Sqrt(dcaX[k]*dcaX[k]+dcaY[k]*dcaY[k]+dcaZ[k]*dcaZ[k]));
      chisqXY->Fill(ChiSqXY[k]);
      chiSqZ->Fill(ChiSqZ[k]);	
      if (NHitsFitTpc[k] < 15) continue;
      NoTracks15++;
      dca15->Fill(TMath::Sqrt(dcaX[k]*dcaX[k]+dcaY[k]*dcaY[k]+dcaZ[k]*dcaZ[k]));
      chisqXY15->Fill(ChiSqXY[k]);
      chiSqZ15->Fill(ChiSqZ[k]);	
    }
    prVSgl->Fill(NtpcG,Ntpc);
    prVSgl15->Fill(NoTracksG15,NoTracks15);
    if (NoTracks > 0) {
      ratZG->Fill(vertexZ[0],(0.99*NoTracks)/(0.99*NoTracksG));
      if (NoTracksG15) ratZG15->Fill(vertexZ[0],(0.99*NoTracks15)/(0.99*NoTracksG15));
    }
    //    iter.Reset(); //ready for next loop                                 
  }
  if (fOut) {
    fOut->cd();
#if 0
    TH1D* pTP = pTfitP->ProjectionX("pTP",10,100);
    TH1D* pTN = pTfitN->ProjectionX("pTN",10,100);
    TH1D* ratio = new TH1D(*pTP);
    ratio->SetName("ratio");
    ratio->SetTitle("ratio positive to negative");
    ratio->Divide(pTN);
#endif
    fOut->Write();
  }
}
//________________________________________________________________________________
void muDst(Int_t kase=-1) {
  Char_t *Run[] = {
    "/star/data32/reco/productionMinBias/FullField",
    "/star/data32/reco/productionMinBias/ReversedFullField",
    "/star/data36/reco/productionHalfHigh/HalfField",
    "/star/data36/reco/productionHalfLow/HalfField",
    "/star/data36/reco/productionHigh/FullField",
    "/star/data36/reco/productionHigh/ReversedFullField",
    "/star/data36/reco/productionLow/FullField",
    "/star/data36/reco/productionLow/ReversedFullField",
    "/star/data36/reco/productionMid/ReversedFullField",
    "/star/data36/reco/productionMinBias/FullField",
    "/star/data36/reco/productionMinBias/ReversedFullField",
    "/star/data37/reco/productionHalfHigh/HalfField",
    "/star/data37/reco/productionHalfLow/HalfField",
    "/star/data37/reco/productionHigh/FullField",
    "/star/data37/reco/productionHigh/ReversedFullField",
    "/star/data38/reco/productionHalfLow/HalfField",
    "/star/data38/reco/productionHigh/FullField",
    "/star/data38/reco/productionHigh/ReversedFullField"};
  Int_t NRuns = sizeof(Run)/sizeof(Char_t*);
  cout << "Request set " << kase << " from total " << NRuns << endl;
  if (kase < 0 || kase >= NRuns) return;
  TString run(Run[kase]);
  const Char_t *fieldset = gSystem->BaseName(run.Data()); cout << "fieldset " << fieldset << endl;
  const Char_t *Dir      = gSystem->DirName(run.Data());
  const Char_t *Production = gSystem->BaseName(Dir);
  run += "/P05ia.ittf/2004/*/*.MuDst.root";
  TString Output(Production); cout << Output << endl;
  Output += "_";              cout << Output << endl;
  Output += fieldset;         cout << Output << endl;
  Output += "_C";
  Output += ".root";          cout << Output << endl;
  muDst(run.Data(),Output.Data());
}
