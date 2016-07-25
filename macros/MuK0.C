#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#endif
//________________________________________________________________________________
void MuK0(const Char_t *files =  "*.MuDst.root", const Char_t *Out="K0.root") {
  TFile *fOut = new TFile(Out,"recreate");
  TH2D *PrVsGl = new TH2D("PrVsGl","No. of Primary tracks versus No. of Globals",
			    100,0,5000., 100,0.,2500.);
  TH1D *K0mass = new TH1D("K0mass","mass of K0",200,.400,.600);
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  const Int_t    &NoTracksGl       = iter("GlobalTracks");
  const UChar_t *&NHitsGl          = iter("GlobalTracks.mNHits");   
  const UChar_t *&NHitsDedxGl      = iter("GlobalTracks.mNHitsDedx");   
  const Float_t *&dEdxGl           = iter("GlobalTracks.mdEdx");   
  const Float_t *&dEdxFitGl        = iter("GlobalTracks.mProbPidTraits.mdEdxFit");   
  const Float_t *&pTGl             = iter("GlobalTracks.mPt");   
  const Float_t *&EtaGl            = iter("GlobalTracks.mEta");   

  const Int_t    &NoTracksPr = iter("PrimaryTracks");
  const UChar_t *&NHitsPr          = iter("PrimaryTracks.mNHits");   
  const UChar_t *&NHitsDedxPr      = iter("PrimaryTracks.mNHitsDedx");   
  const Float_t *&dEdxPr           = iter("PrimaryTracks.mdEdx");   
  const Float_t *&dEdxFitPr        = iter("PrimaryTracks.mProbPidTraits.mdEdxFit");   
  const Float_t *&pTPr             = iter("PrimaryTracks.mPt");   
  const Float_t *&EtaPr            = iter("PrimaryTracks.mEta");   
  const Int_t   &NoV0              = iter("V0");
  const Float_t *&mMomPosX  = iter("V0.mMomPosX");
  const Float_t *&mMomPosY  = iter("V0.mMomPosY");
  const Float_t *&mMomPosZ  = iter("V0.mMomPosZ");
  const Float_t *&mMomNegX  = iter("V0.mMomNegX");
  const Float_t *&mMomNegY  = iter("V0.mMomNegY");
  const Float_t *&mMomNegZ  = iter("V0.mMomNegZ");
  
  //         Now iterations
  while (iter.Next()) {
#ifdef DEBUG
    cout << "NoTracks\t" << (int) NoTracksGl << "\t" <<  (int) NoTracksPr << endl;
#endif
    PrVsGl->Fill(NoTracksGl,NoTracksPr);
    Double_t p4[4];
    static const Float_t pimass = 0.13956995;
    for (Int_t k = 0; k < NoV0; k++) {
      TVector3 p(mMomPosX[k],mMomPosY[k],mMomPosZ[k]);
      TVector3 n(mMomNegX[k],mMomNegY[k],mMomNegZ[k]);
      if (p.Mag() < 1.0 || n.Mag() < 1.0) continue;
      Double_t e = pimass*pimass + p.Mag2();
      e = TMath::Sqrt(e);
      TLorentzVector P(p,e);
      e = pimass*pimass + n.Mag2();
      e = TMath::Sqrt(e);
      TLorentzVector N(n,e);
      P += N;
      K0mass->Fill(P.M());
    }
  }
  fOut->Write();
}
