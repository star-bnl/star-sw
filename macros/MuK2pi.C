//#define GlobalTracks_type
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
#include "StBichsel/Bichsel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TLorentzVector.h"
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
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
const Double_t zcut = 5;
#define __PROB_SCALE__  1000.
#define __SIGMA_SCALE__ 1000.
#define __NOVALUE__     -999
static const Double_t amPi = 0.13956995;
static const Double_t amP  = 0.93827231;
static const Double_t amK  = 0.4936770;
//________________________________________________________________________________
void MuK2pi(const Char_t *files="*.MuDst.root", 
	   const Char_t *Out="MuK2pi.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("MuDst");
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString dir = file1;
  dir.ReplaceAll(".MuDst.root","");
  dir.ReplaceAll("st_physics_","");
  dir.ReplaceAll("raw_","");
  dir.ReplaceAll("_adc_","");
  dir.ReplaceAll(".","");
  dir.ReplaceAll("/","");
  dir.ReplaceAll("star","");
  dir.ReplaceAll("direct","");
  dir.ReplaceAll("+","");
  dir.ReplaceAll("data","");
  dir.ReplaceAll("reco","");
  dir.ReplaceAll("calib","");
  dir.ReplaceAll("fisyak","");
  dir.ReplaceAll("SvtSsdAlignment","");
  dir.ReplaceAll("K2pi","");
  TString output(Out);
  output.ReplaceAll(".root","");
  output += dir;
  output += ".root";
  cout << "Output for " << output << endl;
  const Int_t    &NoPrimVertex               = iter("PrimaryVertices");
  //  const Float_t *&PrimVertexX                = iter("PrimaryVertices.mPosition.mX1");
  //  const Float_t *&PrimVertexY                = iter("PrimaryVertices.mPosition.mX2");
  const Float_t *&PrimVertexZ                = iter("PrimaryVertices.mPosition.mX3");
  const Float_t *&PrimVerSigX                = iter("PrimaryVertices.mPosError.mX1");
  const Float_t *&PrimVerSigY                = iter("PrimaryVertices.mPosError.mX2");
  const Float_t *&PrimVerSigZ                = iter("PrimaryVertices.mPosError.mX3");
  const Int_t    &NoTracks                   = iter("PrimaryTracks");
  const Int_t   *&PrimaryTracks_mVertexIndex = iter("PrimaryTracks.mVertexIndex");
  const UChar_t *&mNHitsFitInner             = iter("PrimaryTracks.mNHitsFitInner");
  const UChar_t *&mNHitsPossInner            = iter("PrimaryTracks.mNHitsPossInner");
  //  const UChar_t *&mNHitsFit                  = iter("PrimaryTracks.mNHitsFit");
  const Float_t *&pT                         = iter("PrimaryTracks.mPt");
  const Float_t *&pX                         = iter("PrimaryTracks.mP.mX1");
  const Float_t *&pY                         = iter("PrimaryTracks.mP.mX2");
  const Float_t *&pZ                         = iter("PrimaryTracks.mP.mX3");

  const Short_t *&Q                          = iter("PrimaryTracks.mHelix.mQ");
  //  const Int_t   *&NSigmaElectron             = iter("PrimaryTracks.mNSigmaElectron");
  const Int_t   *&NSigmaPion                 = iter("PrimaryTracks.mNSigmaPion");
  const Int_t   *&NSigmaKaon                 = iter("PrimaryTracks.mNSigmaKaon");
  //  const Int_t   *&NSigmaProton               = iter("PrimaryTracks.mNSigmaProton");
  const Float_t *&dEdxTrackLength            = iter("PrimaryTracks.mProbPidTraits.mdEdxTrackLength");
  Float_t ppp;
  //  const Float_t *&Eta                        = iter("PrimaryTracks.mEta");
  const Float_t *&Phi                        = iter("PrimaryTracks.mPhi");

  const   Float_t   *&mDcaD                  = iter("PrimaryTracks.mDcaD");
  const   Float_t   *&mSigmaOfDcaD           = iter("PrimaryTracks.mSigmaOfDcaD");
  const   Float_t   *&mDcaZ                  = iter("PrimaryTracks.mDcaZ");
  const   Float_t   *&mSigmaOfDcaZ           = iter("PrimaryTracks.mSigmaOfDcaZ");
  TFile *fOut = new TFile(output,"recreate");
  TString zCut(Form(" vs no. of Possible ones for primary tracks with primary vertex |Z| < %f cm", zcut));
  TString Name;
  TString Title;
  Name = "NoSvtHits"; Title = "No.of fitted SVT hits"; Title += zCut;
  TH2D *NoSvtHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoSsdHits"; Title = "No.of fitted SSD hits"; Title += zCut;
  TH2D *NoSsdHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoSvtSsdHits"; Title = "No.of fitted Svt and SSD hits"; Title += zCut;
  TH2D *NoSvtSsdHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  TH2D *dcaXYInvpT[5], *dcaXYPhi[5];
  TH2D *dcaXYInvp[5];
  TH2D *dcaZInvp[5];
  TH2D *dcaZInvpT[5], *dcaZPhi[5];
  TH2D *pullXYInvpT[5], *pullXYPhi[5];
  TH2D *pullZInvpT[5], *pullZPhi[5];
  TH2D *lXYNSvtSsdHits;
  TH2D *pullXYNSvtSsdHits;
  for (Int_t i = 0; i < 5; i++) {
    TString Selection;
    if (i == 0)      Selection = "All";
    else if (i == 4) Selection = "No.Svt+Ssd >= 4";
    else             Selection = Form("No. Svt+Ssd = %i",i);
    Name = Form("dcaXYInvpT%i",i); Title = "dca XY versus 1/pT for "; Title += Selection;
    dcaXYInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("dcaXYInvp%i",i); Title = "dca XY versus 1/p for "; Title += Selection;
    dcaXYInvp[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("dcaXYPhi%i",i); Title = "dca XY versus Phi for pT > 1 GeV/c and "; Title += Selection;
    dcaXYPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -1., 1.);
    Name = Form("dcaZInvpT%i",i); Title = "dca Z versus 1/pT for "; Title += Selection;
    dcaZInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("dcaZInvp%i",i); Title = "dca Z versus 1/p for "; Title += Selection;
    dcaZInvp[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("dcaZPhi%i",i); Title = "dca Z versus Phi for pT > 1 GeV/c and "; Title += Selection;
    dcaZPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -1., 1.);
    Name = Form("pullXYInvpT%i",i); Title = "pull XY versus 1/pT for "; Title += Selection;
    pullXYInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullXYPhi%i",i); Title = "pull XY versus Phi for pT > 1 GeV/c and "; Title += Selection;
    pullXYPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -20., 20.);
    Name = Form("pullZInvpT%i",i); Title = "pull Z versus 1/pT for "; Title += Selection;
    pullZInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullZPhi%i",i); Title = "pull Z versus Phi for pT > 1 GeV/c and "; Title += Selection;
    pullZPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -20., 20.);
    if (i == 0) {
      Name = Form("lXYNSvtSsdHits"); Title = "l XY versus total no. of Svt and Ssd hits "; Title += Selection;
      lXYNSvtSsdHits = new TH2D(Name,Title,20,0,20, 500, -2.5, 2.5);
      Name = Form("pullXYNSvtSsdHits"); Title = "l XY versus total no. of Svt and Ssd hits "; Title += Selection;
      pullXYNSvtSsdHits = new TH2D(Name,Title,20,0,20, 500, -25, 25);
    }
  }
  Int_t nBinsDCA = 400;
  Double_t maxDCA   =  20;
  TH2D *NvsDCAxy = new TH2D("NvsDCAxy","No. of tracks with 2 or more precise hits  per event with s.t.d for dcaXY  more than X",
			    100,0,100,nBinsDCA,0,maxDCA);
  TH2D *NvsDCAz  = new TH2D("NvsDCAz" ,"No. of tracks with 2 or more precise hits  per event with s.t.d for dcaZ  more than X",
			    100,0,100,nBinsDCA,0,maxDCA);
  TH2D *NvsDCA   = new TH2D("NvsDCA"  ,"No. of tracks with 2 or more precise hits  per event with s.t.d for total dca  more than X",
			    100,0,100,nBinsDCA,0,maxDCA);
  TH1D *DCAxy = new TH1D("DCAxy","No. of tracks with 2 or more precise hits  per event with s.t.d for dcaXY  more than X",nBinsDCA,0,maxDCA);
  TH1D *DCAz  = new TH1D("DCAz","No. of tracks with 2 or more precise hits  per event with s.t.d for dcaz  more than X",nBinsDCA,0,maxDCA);
  TH1D *DCA   = new TH1D("DCA","No. of tracks with 2 or more precise hits  per event with s.t.d for dca  more than X",nBinsDCA,0,maxDCA);
  struct Plot_t {
    Char_t *Name;
    Char_t *Title;
  };
  const Int_t Nsys = 2;
  const Plot_t SysNames[Nsys] = {
    {"KPpi","K+pi-pi-"},
    {"KNpi","K-pi+pi+"}
  };
  const Int_t Ntyp= 2;
  const Plot_t SysTypes[Ntyp] = {
    {"lXY","|XY decay length|"},
    {"Eff","Effective mass"}
  };
  // Separated cuts
  const Int_t Nl = 2;
  const Plot_t LName[Nl] = {
    {"lXYP","lXY > 0"},
    {"lXYN","lXY < 0"},
  };
  const Int_t Nm = 2;
  const Plot_t TName[Nm] = {
    {"GJP","cos(Theta_{GJ}) > 0"},
    {"GJN","cos(Theta_{GJ}) < 0"},
  };
  // commulicative cuts
  const Int_t Ncut = 10;
  const Plot_t CutNames[Ncut] = {
    {"","No cuts"},
    {"S","no.of SSD+SVT hit for both tracks > 1"},
    {"Sdca","dca/sigma_dca > 3 for both tracks"},
    {"dEdx","n_{Sigma} < 2"},
    {"t2","more than 2 tracks with dca s.t.d. > 2"},
    {"t3","more than 2 tracks with dca s.t.d. > 3"},
    {"t4","more than 2 tracks with dca s.t.d. > 4"},
    {"t5","more than 2 tracks with dca s.t.d. > 5"},
    {"t6","more than 2 tracks with dca s.t.d. > 6"},
    {"t7","more than 2 tracks with dca s.t.d. > 7"}
  };
  TH1D *hists[Nsys][Ntyp][Nl][Nm][Ncut];
  memset (hists, 0, Nsys*Ntyp*Nl*Nm*Ncut*sizeof(TH1D *));
  
  static const Double_t  MinMass = amK + amPi + amPi;
  static const Double_t  MaxMass = MinMass + 2;
  for (Int_t s = 0; s < Nsys; s++) {
    for (Int_t t = 0; t < Ntyp; t++) {
      Int_t  nx = 200;
      Double_t xmin = 0, xmax = 8;
      if (t != 0) {
	nx   = 1000;
	xmin = MinMass;
	xmax = MaxMass;
      }
      for (Int_t l = 0; l < Nl; l++) {
	for (Int_t m = 0; m < Nm; m++) {
	  TString Cut, CutN;
	  for (Int_t c = 0; c < Ncut; c++) {
	    Name  = SysNames[s].Name; Title                = SysNames[s].Title;
	    Name += SysTypes[t].Name; Title += " "; Title += SysTypes[t].Title;
	    Name += LName[l].Name; Title += " "; Title += LName[l].Title;
	    Name += TName[m].Name; Title += " "; Title += TName[m].Title;
	    Name += CutNames[c].Name; Title += " "; 
	    if (c == 0 || c == 1) Cut = CutNames[c].Title;
	    else {Cut += " "; Cut += CutNames[c].Title;}
	    if (c < 4) CutN = Cut;
	    else {Cut = CutN; Cut += CutNames[c].Title;}
	    Title += Cut;
	    hists[s][t][l][m][c] = new TH1D(Name,Title,nx,xmin,xmax);
	  }
	}
      }
    }
  }
  //         Now iterations
  Int_t NevProc = 0;
  //  Int_t TreeNo = -1;
  //  TString currentFile("");
  while (iter.Next()) {
//     if (TreeNo != fChain->GetTreeNumber()) {
//        cout << "Read event \t" << jentry 
// 	    << " so far. switch to file " << fChain->GetCurrentFile()->GetName() << endl;
//        TreeNo = fChain->GetTreeNumber();
//      }

    NevProc++;
    for (Int_t l = 0; l < NoPrimVertex; l++) {
      if (TMath::Abs(PrimVertexZ[l]) > zcut ) continue;
      DCAxy->Reset();
      DCAz->Reset();
      DCA->Reset();
      //      if (TMath::Abs(PrimVertexZ[l]) <= 25. ) continue;
      Int_t NtrkDcaStd[6];
      memset(NtrkDcaStd, 0, 6*sizeof(Int_t));
      for (Int_t k = 0; k<NoTracks; k++) {
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	//	if (mNHitsFit[k] < 25) continue;
	//	if (pT[k] < 0.1) continue;
	//	if ( (pZ[k]/pT[k])>= 1. ) continue;
	Int_t NoFSvtHits =  (mNHitsFitInner[k] & 0x7);
	Int_t NoFSsdHits = ((mNHitsFitInner[k] & 0x18) >> 3);
	Int_t NoFSvtSsdHits = NoFSvtHits + NoFSsdHits;
	Int_t NoPSvtHits =  (mNHitsPossInner[k] & 0x7);
	Int_t NoPSsdHits = ((mNHitsPossInner[k] & 0x18) >> 3);
	Int_t NoPSvtSsdHits = NoPSvtHits + NoPSsdHits;
	NoSvtHits->Fill(NoPSvtHits,NoFSvtHits);
	NoSsdHits->Fill(NoPSsdHits,NoFSsdHits);
	NoSvtSsdHits->Fill(NoPSvtSsdHits,NoFSvtSsdHits);
	Double_t phi = 180*Phi[k]/TMath::Pi();
	ppp = sqrt(pZ[k]*pZ[k]+pZ[k]*pZ[k]);
	Int_t N = 0;
	N = NoFSvtSsdHits;
#if 0
	cout << "Primary l = " << l 
	     << " x " << PrimVertexX[l] << " +/- " << PrimVerSigX[l]
	     << " y " << PrimVertexY[l] << " +/- " << PrimVerSigY[l]
	     << " z " << PrimVertexZ[l] << " +/- " << PrimVerSigZ[l] << endl;
#endif
	Double_t sigmaXY = TMath::Sqrt(mSigmaOfDcaD[k]*mSigmaOfDcaD[k] + 
				       PrimVerSigX[l]*PrimVerSigX[l] +  
				       PrimVerSigY[l]*PrimVerSigY[l]);
	Double_t sigmaZ = TMath::Sqrt(mSigmaOfDcaZ[k]*mSigmaOfDcaZ[k] + PrimVerSigZ[l]*PrimVerSigZ[l]);
#if 0
	cout << "Sigma DCA xy " << mSigmaOfDcaD[k] << " => " << sigmaXY << endl;
	cout << "Sigma DCA z  " << mSigmaOfDcaZ[k] << " => " << sigmaZ << endl;
#endif
	Double_t dcaXY = mDcaD[k];
	Double_t dcaZ  = mDcaZ[k];
	Double_t pullXY = -999;
	Double_t pullZ  = -999;
	Double_t pull   = -999;
	if (sigmaXY > 0) pullXY = dcaXY/sigmaXY;
	if (sigmaZ  > 0) pullZ  = dcaZ /sigmaZ;
	if (pullXY > -999 && pullZ > -999) pull = TMath::Sqrt(pullXY*pullXY + pullZ*pullZ);
	if (N == 0)	  {
	  dcaXYInvpT[N]->Fill(1./pT[k],dcaXY);
	  dcaXYInvp[N]->Fill(1./ppp,dcaXY);
	  if (pullXY > -999) pullXYInvpT[N]->Fill(1./pT[k],pullXY);
	  dcaZInvpT[N]->Fill(1./pT[k],pullZ);
	  dcaZInvp[N]->Fill(1./ppp,pullZ);
	  if (pullZ  > -999) pullZInvpT[N]->Fill(1./pT[k],pullZ);
	  if (pT[k] > 1.) {
	    dcaXYPhi[N]->Fill(phi,dcaXY);
	    if (pullXY > -999) pullXYPhi[N]->Fill(phi,pullXY);
	    dcaZPhi[N]->Fill(phi,pullZ);
	    if (pullZ  > -999) pullZPhi[N]->Fill(phi,pullZ);
	    }
	}
	
	if (N > 4) N = 4;
	if (N > 0) {
	  dcaXYInvpT[N]->Fill(1./pT[k],dcaXY);
	  dcaXYInvp[N]->Fill(1./ppp,dcaXY);
	  if (pullXY > -999) pullXYInvpT[N]->Fill(1./pT[k],pullXY);
	  dcaZInvpT[N]->Fill(1./pT[k],pullZ);
	  dcaZInvp[N]->Fill(1./ppp,pullZ);
	  if (pullZ  > -999) pullZInvpT[N]->Fill(1./pT[k],pullZ);
	  if (pT[k] > 1.) {
	    dcaXYPhi[N]->Fill(phi,dcaXY);
	    if (pullXY > -999) pullXYPhi[N]->Fill(phi,pullXY);
	    dcaZPhi[N]->Fill(phi,pullZ);
	    if (pullZ  > -999) pullZPhi[N]->Fill(phi,pullZ);
	  }
	}
	if (N >= 2) {
	  if (pullXY  > - 999) DCAxy->Fill(TMath::Abs(pullXY));
	  if (pullZ   > - 999) DCAz->Fill(TMath::Abs(pullZ));
	  if (pull    > - 999) DCA->Fill(pull);
	}
	if (DCA->GetEntries() > 0) {
	  Double_t nXY = 0;
	  Double_t nZ  = 0;
	  Double_t n   = 0;
	  for (Int_t i = nBinsDCA; i > 0; i--) {
	    Double_t x =  DCA->GetBinCenter(i);
	    nXY += DCAxy->GetBinContent(i);
	    nZ  += DCAz->GetBinContent(i);
	    n += DCA->GetBinContent(i);
	    if (nXY > 0) {
	      NvsDCAxy->Fill(nXY,x);
	    }
	    if (nZ  > 0) {
	      NvsDCAz->Fill(nZ,x);
	    }
	    if (n   > 0) {
	      NvsDCA->Fill(n,x);
	      if (x > 2) NtrkDcaStd[0]++;
	      if (x > 3) NtrkDcaStd[1]++;
	      if (x > 4) NtrkDcaStd[2]++;
	      if (x > 5) NtrkDcaStd[3]++;
	      if (x > 6) NtrkDcaStd[4]++;
	      if (x > 7) NtrkDcaStd[5]++;
	    }
	  }
	}
      }
      // pairs
      Double_t dcaXY[3];
      Double_t dcaZ[3];
      Double_t pull[3];
      Double_t pullXY[3];
      Double_t pullZ[3];
      Double_t sigmaXY[3];
      Double_t sigmaZ[3];
      Double_t dev[3];
      Int_t    NoFSvtSsdHits[3];
      Double_t delPhi[3];
      for (Int_t k = 0; k<NoTracks; k++) {
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	dcaXY[0] = mDcaD[k];
	dcaZ[0]  = mDcaZ[k];
	pullXY[0] = -999;
	pullZ[0]  = -999;
	pull[0]   = -999;
	sigmaXY[0] = TMath::Sqrt(mSigmaOfDcaD[k]*mSigmaOfDcaD[k] + 
	       	       PrimVerSigX[l]*PrimVerSigX[l] +  
	       	       PrimVerSigY[l]*PrimVerSigY[l]);
	sigmaZ[0] = TMath::Sqrt(mSigmaOfDcaZ[k]*mSigmaOfDcaZ[k] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	if (sigmaXY[0] > 0) pullXY[0] = dcaXY[0]/sigmaXY[0];
	if (sigmaZ[0]  > 0) pullZ[0]  = dcaZ[0] /sigmaZ[0];
	if (pullXY[0] > -999 && pullZ[0] > -999) pull[0] = TMath::Sqrt(pullXY[0]*pullXY[0] + pullZ[0]*pullZ[0]);
	TVector3 trK(pX[k],pY[k],pZ[k]);
	Double_t pmom2 = trK.Mag2();
	Double_t energy = TMath::Sqrt(amK*amK + pmom2);
	TLorentzVector K(trK,energy);
	dev[0] = NSigmaKaon[k]/__SIGMA_SCALE__;
	Int_t  s = 0;
	if  (Q[k] < 0) s = 1;
	NoFSvtSsdHits[0] = (mNHitsFitInner[k] & 0x7) + ((mNHitsFitInner[k] & 0x18) >> 3); 
	for (Int_t i = 1; i < NoTracks; i++) {
	  if (PrimaryTracks_mVertexIndex[i] != l) continue;
	  if (k    ==   i ) continue;
	  //	  if (mDcaD[k]*mDcaD[i] > 0 ) continue;
	  sigmaXY[1] = TMath::Sqrt(mSigmaOfDcaD[i]*mSigmaOfDcaD[i] + 
				PrimVerSigX[l]*PrimVerSigX[l] +  
				PrimVerSigY[l]*PrimVerSigY[l]);
	  sigmaZ[1] = TMath::Sqrt(mSigmaOfDcaZ[i]*mSigmaOfDcaZ[i] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	  dcaXY[1] = mDcaD[i];
	  dcaZ[1]  = mDcaZ[i];
	  pull[1]  = -999;
	  if (sigmaXY[1] > 0) pullXY[1] = dcaXY[1]/sigmaXY[1];
	  if (sigmaZ[1]  > 0) pullZ[1]  = dcaZ[1] /sigmaZ[1];
	  if (pullXY[1] > -999 && pullZ[1] > -999) pull[1] = TMath::Sqrt(pullXY[1]*pullXY[1] + pullZ[1]*pullZ[1]);
	  NoFSvtSsdHits[1] = (mNHitsFitInner[i] & 0x7) + ((mNHitsFitInner[i] & 0x18) >> 3); 
	  dev[1] = NSigmaPion[i]/__SIGMA_SCALE__;
	  TVector3 trI(pX[i],pY[i],pZ[i]);
	  pmom2 = trI.Mag2();
	  energy = TMath::Sqrt(amPi*amPi + pmom2);
	  TLorentzVector piI(trI,energy);
	  for (Int_t j = 0; j < i; j++) {
	    if (PrimaryTracks_mVertexIndex[j] != l) continue;
	    if (k    ==   j ) continue;
	    if (Q[k] != -Q[i] || Q[k] != -Q[j]) continue; // K-+pi+-pi+-
	    //	  if (mDcaD[k]*mDcaD[j] > 0 ) continue;
	    sigmaXY[2] = TMath::Sqrt(mSigmaOfDcaD[j]*mSigmaOfDcaD[j] + 
				PrimVerSigX[l]*PrimVerSigX[l] +  
				PrimVerSigY[l]*PrimVerSigY[l]);
	    sigmaZ[2] = TMath::Sqrt(mSigmaOfDcaZ[j]*mSigmaOfDcaZ[j] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	    dcaXY[2] = mDcaD[j];
	    dcaZ[2]  = mDcaZ[j];
	    pull[2]  = -999;
	    if (sigmaXY[2] > 0) pullXY[2] = dcaXY[2]/sigmaXY[2];
	    if (sigmaZ[2]  > 0) pullZ[2]  = dcaZ[2] /sigmaZ[2];
	    if (pullXY[2] > -999 && pullZ[2] > -999) pull[2] = TMath::Sqrt(pullXY[2]*pullXY[2] + pullZ[2]*pullZ[2]);
	    NoFSvtSsdHits[2] = (mNHitsFitInner[j] & 0x7) + ((mNHitsFitInner[j] & 0x18) >> 3); 
	    dev[2] = NSigmaPion[j]/__SIGMA_SCALE__;
	    TVector3 trJ(pX[j],pY[j],pZ[j]);
	    pmom2 = trJ.Mag2();
	    energy = TMath::Sqrt(amPi*amPi + pmom2);
	    TLorentzVector piJ(trJ,energy);
	    
	    TLorentzVector Kpipi(piJ);
	    Kpipi += piI;
	    Kpipi += K;
	    TVector3 bF = Kpipi.BoostVector();
	    TVector3 b(-bF.X(),-bF.Y(),-bF.Z());
	    TLorentzVector Kl(K);
	    Kl.Boost(b);
	    Double_t Theta_GJ_K = Kl.CosTheta(); 
	    Double_t M = Kpipi.M();// cout << "M\t" << M;
	    if (M > MaxMass) continue;
	    //	  TVector3 K3 = K.Vect().Cross(pi.Vect());
	    delPhi[0] = Kpipi.DeltaPhi(K);
	    delPhi[1] = Kpipi.DeltaPhi(piI);
	    delPhi[2] = Kpipi.DeltaPhi(piJ);
	    Double_t lijkXY[3] = {0, 0, 0};
	    if (TMath::Abs(delPhi[0]) >  1.e-7)  lijkXY[0] =  mDcaD[k]/TMath::Sin(delPhi[0]);
	    if (TMath::Abs(delPhi[1]) >  1.e-7)  lijkXY[1] =  mDcaD[i]/TMath::Sin(delPhi[1]);
	    if (TMath::Abs(delPhi[2]) >  1.e-7)  lijkXY[2] =  mDcaD[j]/TMath::Sin(delPhi[2]);
	    Double_t lXY = 0;
	    Double_t sigma2lXY = 0;
	    for (Int_t m = 0; m < 3; m++) {
	      if (TMath::Abs(delPhi[m]) >  1.e-7)  {
		lijkXY[m] =  dcaXY[m]/TMath::Sin(delPhi[m]);
		if (sigmaXY[m] > 0) {
		  Double_t sigma2XY = sigmaXY[m]*sigmaXY[m];
		  lXY       += lijkXY[m]/sigma2XY;
		  sigma2lXY +=        1./sigma2XY;
		}
	      }
	    }
	    Double_t sigmalXY = 0;
	    if (sigma2lXY > 0) {
	      lXY /= sigma2lXY;
	      sigmalXY = 1./TMath::Sqrt(sigma2lXY);
	      lXYNSvtSsdHits->Fill(NoFSvtSsdHits[0]+NoFSvtSsdHits[1]+NoFSvtSsdHits[2],lXY);
	      pullXYNSvtSsdHits->Fill(NoFSvtSsdHits[0]+NoFSvtSsdHits[1]+NoFSvtSsdHits[2],lXY/sigmalXY);
	    }
	    Double_t var[2] = {TMath::Abs(lXY), M};
	    for (Int_t l = 0; l < Nl; l++) {
	      if (l == 0 && lXY < 0 || l == 1 && lXY > 0) continue;
	      for (Int_t m = 0; m < Nm; m++) {
		if (m == 0 && Theta_GJ_K < 0 || m == 1 && Theta_GJ_K > 0) continue;
		for (Int_t  c = 0; c < Ncut; c++) {
		  //"","No cuts"},
		  if (c == 1 && ! (NoFSvtSsdHits[0] > 1 && NoFSvtSsdHits[1] > 1)) break; //"S","no.of SSD+SVT hit for both tracks > 1"},
		  if (c == 2 && ! (TMath::Abs(pull[0]) > 3 && TMath::Abs(pull[1]) > 3 && TMath::Abs(pull[2]) > 3)) break;
		      //"Sdca","dca/sigma_dca > 3 for both tracks"},
		  if (c == 3 && 
		      ! (dEdxTrackLength[k] > 40 && dEdxTrackLength[i] > 40 && 
			 TMath::Abs(dev[0]) > 2 && TMath::Abs(dev[1]) > 2 && TMath::Abs(dev[2]) > 2)) break;//"dEdx","n_{Sigma} < 2"}
		  if (c > 3 && NtrkDcaStd[c-4] <= 2) break;
		  for (Int_t t = 0; t < Ntyp; t++) {
		    hists[s][t][l][m][c]->Fill(var[t]);
		  }
		}
	      }
	    }
	  }
	}
      }
    }
    if (NevProc%10000 == 1) cout << NevProc << "\tevents processed so far" << endl;
  }
  fOut->Write();
  delete fOut;
}
  
