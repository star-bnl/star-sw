//#define DEBUG
//#define __OLD_DCA__
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
#include "StThreeVectorF.hh"
#include "THelixTrack.h"
#include "TMath.h"
#include "StDcaGeometry.h"
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
//class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
const Double_t zcut = 5;
//________________________________________________________________________________
void MuSvt(const Char_t *files="*.MuDst.root", const Char_t *Out="MuSvtOut.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("MuDst");
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    TString dir = gSystem->DirName(file1);
    dir.ReplaceAll(".","");
    dir.ReplaceAll("/","");
    dir.ReplaceAll("star","");
    dir.ReplaceAll("data","");
    dir.ReplaceAll("*","");
    dir.ReplaceAll("reco","");
    output.ReplaceAll(".root","");
    output += dir;
    output += ".root";
  }
  cout << "Output for " << output << endl;
  const Int_t    &NoPrimVertex               = iter("PrimaryVertices");
  const Float_t *&PrimVertexX                = iter("PrimaryVertices.mPosition.mX1");
  const Float_t *&PrimVertexY                = iter("PrimaryVertices.mPosition.mX2");
  const Float_t *&PrimVertexZ                = iter("PrimaryVertices.mPosition.mX3");
  const Float_t *&PrimVerSigX                = iter("PrimaryVertices.mPosError.mX1");
  const Float_t *&PrimVerSigY                = iter("PrimaryVertices.mPosError.mX2");
  const Float_t *&PrimVerSigZ                = iter("PrimaryVertices.mPosError.mX3");

  const Int_t    &NoTracks                   = iter("PrimaryTracks");
  const Int_t   *&PrimaryTracks_mVertexIndex = iter("PrimaryTracks.mVertexIndex");
  const UChar_t *&mNHitsFitInner             = iter("PrimaryTracks.mNHitsFitInner");
  const UChar_t *&mNHitsPossInner            = iter("PrimaryTracks.mNHitsPossInner");
  const UChar_t *&mNHitsFit                  = iter("PrimaryTracks.mNHitsFit");
  const UShort_t*&   PrimaryVertices_mRefMultNeg              = iter("PrimaryVertices.mRefMultNeg");
  const UShort_t*&   PrimaryVertices_mRefMultPos              = iter("PrimaryVertices.mRefMultPos");

  const Float_t *&pT                         = iter("PrimaryTracks.mPt");
  const Float_t *&pX                         = iter("PrimaryTracks.mP.mX1");
  const Float_t *&pY                         = iter("PrimaryTracks.mP.mX2");
  const Float_t *&pZ                         = iter("PrimaryTracks.mP.mX3");
  const Float_t *&Eta                        = iter("PrimaryTracks.mEta");
  const Float_t *&Phi                        = iter("PrimaryTracks.mPhi");
#ifdef __OLD_DCA__
  const   Float_t   *&mSigmaDcaD             = iter("PrimaryTracks.mSigmaDcaD");
  const   Float_t   *&mSigmaDcaZ             = iter("PrimaryTracks.mSigmaDcaZ");
#endif
  const Int_t   *&mIndex2Global              = iter("PrimaryTracks.mIndex2Global");
//   const Float_t*&    mDCA_mX1                   = iter("PrimaryTracks.mDCA.mX1");
//   const Float_t*&    mDCA_mX2                   = iter("PrimaryTracks.mDCA.mX2");
//   const Float_t*&    mDCA_mX3                   = iter("PrimaryTracks.mDCA.mX3");
//   const Float_t*&    mDCAGlobal_mX1             = iter("PrimaryTracks.mDCAGlobal.mX1");
//   const Float_t*&    mDCAGlobal_mX2             = iter("PrimaryTracks.mDCAGlobal.mX2");
//   const Float_t*&    mDCAGlobal_mX3             = iter("PrimaryTracks.mDCAGlobal.mX3");

  // Global
  const Int_t    &NoTracksGl                   = iter("GlobalTracks");
  const UChar_t *&mNHitsFitInnerGl             = iter("GlobalTracks.mNHitsFitInner");
  const UChar_t *&mNHitsPossInnerGl            = iter("GlobalTracks.mNHitsPossInner");
  const UChar_t *&mNHitsFitGl                  = iter("GlobalTracks.mNHitsFit");
  const Float_t *&pTGl                         = iter("GlobalTracks.mPt");
  const Float_t *&pXGl                         = iter("GlobalTracks.mP.mX1");
  const Float_t *&pYGl                         = iter("GlobalTracks.mP.mX2");
  const Float_t *&pZGl                         = iter("GlobalTracks.mP.mX3");
  const Float_t *&EtaGl                        = iter("GlobalTracks.mEta");
  const Float_t *&PhiGl                        = iter("GlobalTracks.mPhi");
#ifdef __OLD_DCA__
  const Float_t *&mSigmaDcaDGl                 = iter("GlobalTracks.mSigmaDcaD");
  const Float_t *&mSigmaDcaZGl                 = iter("GlobalTracks.mSigmaDcaZ");
#else
  const Int_t*&      GlobalTracks_mIndex2Cov                  = iter("GlobalTracks.mIndex2Cov");
  const Int_t&       NoCovGlobTrack                           = iter("CovGlobTrack");
  const Float_t*&    CovGlobTrack_mImp                        = iter("CovGlobTrack.mImp");
  const Float_t*&    CovGlobTrack_mZ                          = iter("CovGlobTrack.mZ");
  const Float_t*&    CovGlobTrack_mPsi                        = iter("CovGlobTrack.mPsi");
  const Float_t*&    CovGlobTrack_mPti                        = iter("CovGlobTrack.mPti");
  const Float_t*&    CovGlobTrack_mTan                        = iter("CovGlobTrack.mTan");
  const Float_t*&    CovGlobTrack_mCurv                       = iter("CovGlobTrack.mCurv");
  const Float_t*&    CovGlobTrack_mImpImp                     = iter("CovGlobTrack.mImpImp");
  const Float_t*&    CovGlobTrack_mZImp                       = iter("CovGlobTrack.mZImp");
  const Float_t*&    CovGlobTrack_mZZ                         = iter("CovGlobTrack.mZZ");
  const Float_t*&    CovGlobTrack_mPsiImp                     = iter("CovGlobTrack.mPsiImp");
  const Float_t*&    CovGlobTrack_mPsiZ                       = iter("CovGlobTrack.mPsiZ");
  const Float_t*&    CovGlobTrack_mPsiPsi                     = iter("CovGlobTrack.mPsiPsi");
  const Float_t*&    CovGlobTrack_mPtiImp                     = iter("CovGlobTrack.mPtiImp");
  const Float_t*&    CovGlobTrack_mPtiZ                       = iter("CovGlobTrack.mPtiZ");
  const Float_t*&    CovGlobTrack_mPtiPsi                     = iter("CovGlobTrack.mPtiPsi");
  const Float_t*&    CovGlobTrack_mPtiPti                     = iter("CovGlobTrack.mPtiPti");
  const Float_t*&    CovGlobTrack_mTanImp                     = iter("CovGlobTrack.mTanImp");
  const Float_t*&    CovGlobTrack_mTanZ                       = iter("CovGlobTrack.mTanZ");
  const Float_t*&    CovGlobTrack_mTanPsi                     = iter("CovGlobTrack.mTanPsi");
  const Float_t*&    CovGlobTrack_mTanPti                     = iter("CovGlobTrack.mTanPti");
  const Float_t*&    CovGlobTrack_mTanTan                     = iter("CovGlobTrack.mTanTan");
#endif
  const Float_t*&    mDCA_mX1Gl                    = iter("GlobalTracks.mDCA.mX1");
  //   const Float_t*&    mDCA_mX2Gl                    = iter("GlobalTracks.mDCA.mX2");
  //   const Float_t*&    mDCA_mX3Gl                    = iter("GlobalTracks.mDCA.mX3");
  const Float_t*&    mDCAGlobal_mX1Gl              = iter("GlobalTracks.mDCAGlobal.mX1");
  const Float_t*&    mDCAGlobal_mX2Gl              = iter("GlobalTracks.mDCAGlobal.mX2");
  const Float_t*&    mDCAGlobal_mX3Gl              = iter("GlobalTracks.mDCAGlobal.mX3");
  const Float_t*&    GlobalTracks_mFirstPoint_mX1             = iter("GlobalTracks.mFirstPoint.mX1");
  const Float_t*&    GlobalTracks_mFirstPoint_mX2             = iter("GlobalTracks.mFirstPoint.mX2");
  const Float_t*&    GlobalTracks_mFirstPoint_mX3             = iter("GlobalTracks.mFirstPoint.mX3");
  const Float_t*&    GlobalTracks_mLastPoint_mX1               = iter("GlobalTracks.mLastPoint.mX1");
  const Float_t*&    GlobalTracks_mLastPoint_mX2               = iter("GlobalTracks.mLastPoint.mX2");
  const Float_t*&    GlobalTracks_mLastPoint_mX3               = iter("GlobalTracks.mLastPoint.mX3");

  TFile *fOut = new TFile(output,"recreate");
  TString zCut(Form(" vs no. of Possible ones for primary tracks with primary vertex |Z| < %f cm", zcut));
  TString Name;
  TString Title;
  Name = "NoSvtHits"; Title = "No.of fitted SVT hits"; Title += zCut;
  TH2D *NoSvtHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoSsdHits"; Title = "No.of fitted SSD hits"; Title += zCut;
  TH2D *NoSsdHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoSvtSsdHits"; Title = "No.of fitted Avt and SSD hits"; Title += zCut;
  TH2D *NoSvtSsdHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  TH2D *dcaXYInvp[5], *dcaXYInvpT[5];
  TH2D *dcaZInvp[5],   *dcaZInvpT[5];
  TH2D *dcaXYPhi[5], *dcaZPhi[5];
  TH2D *pullXYPhi[5], *pullZPhi[5];
  TH2D *pullXYInvpT[5], *pullXYInvpTVx[5], *pullZInvpT[5], *pullZInvpTVx[5]; 
  TH2D *pullXYInvp[5],  *pullXYInvpVx[5],  *pullZInvp[5],  *pullZInvpVx[5];
  for (Int_t i = 0; i < 5; i++) {
    TString Selection;
    if (i == 4) Selection = "No.Svt+Ssd >= 4";
    else        Selection = Form("No. Svt+Ssd = %i",i);
    //    Selection += " Global";
    Name = Form("dcaXYInvpT%i",i); Title = "dca XY versus 1/pT for "; Title += Selection;
    dcaXYInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("dcaXYInvp%i",i); Title = "dca XY versus 1/p for "; Title += Selection;
    dcaXYInvp[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("pullXYInvpT%i",i); Title = "pull XY versus 1/pT for "; Title += Selection;
    pullXYInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullXYInvpTVx%i",i); Title = "pull XY with Veretex versus 1/pT for "; Title += Selection;
    pullXYInvpTVx[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullXYInvp%i",i); Title = "pull XY versus 1/p for "; Title += Selection;
    pullXYInvp[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullXYInvpVx%i",i); Title = "pull XY with Veretex versus 1/p for "; Title += Selection;
    pullXYInvpVx[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);

    Name = Form("dcaZInvpT%i",i); Title = "dca Z versus 1/pT for "; Title += Selection;
    dcaZInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("dcaZInvp%i",i); Title = "dca Z versus 1/p for "; Title += Selection;
    dcaZInvp[i] = new TH2D(Name,Title,100,0,10, 500, -1., 1.);
    Name = Form("pullZInvpT%i",i); Title = "pull Z versus 1/pT for "; Title += Selection;
    pullZInvpT[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullZInvpTVx%i",i); Title = "pull Z with Veretex versus 1/pT for "; Title += Selection;
    pullZInvpTVx[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullZInvp%i",i); Title = "pull Z versus 1/p for "; Title += Selection;
    pullZInvp[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);
    Name = Form("pullZInvpVx%i",i); Title = "pull Z with Veretex versus 1/p for "; Title += Selection;
    pullZInvpVx[i] = new TH2D(Name,Title,100,0,10, 500, -20., 20.);


    Name = Form("dcaXYPhi%i",i); Title = "dca XY versus Phi for pT > 1 GeV/c and "; Title += Selection;
    dcaXYPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -1., 1.);
    Name = Form("dcaZPhi%i",i); Title = "dca Z versus Phi for pT > 1 GeV/c and "; Title += Selection;
    dcaZPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -1., 1.);
    Name = Form("pullXYPhi%i",i); Title = "pull XY versus Phi for pT > 1 GeV/c and "; Title += Selection;
    pullXYPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -20., 20.);
    Name = Form("pullZPhi%i",i); Title = "pull Z versus Phi for pT > 1 GeV/c and "; Title += Selection;
    pullZPhi[i] = new TH2D(Name,Title,90,-180,180, 500, -20., 20.);
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
  TH2D *VxSigmaX = new TH2D("VxSigmaX","Vertex sigma X versus log10 no. of good tracks",100,0,5,100,0,0.05);
  TH2D *VxSigmaY = new TH2D("VxSigmaY","Vertex sigma Y versus log10 no. of good tracks",100,0,5,100,0,0.05);
  TH2D *VxSigmaZ = new TH2D("VxSigmaZ","Vertex sigma Z versus log10 no. of good tracks",100,0,5,100,0,0.05);
  TH2D *RefMultZP = new TH2D("RefMultZP","Positive Ref.Mult versus Z",100,-50,50,200,0,2000); 
  TH2D *RefMultZN = new TH2D("RefMultZN","Negative Ref.Mult versus Z",100,-50,50,200,0,2000); 
  //         Now iterations
  while (iter.Next()) {
    for (Int_t l = 0; l < NoPrimVertex; l++) {
#ifdef  DEBUG
      cout << "Primary l = " << l 
	   << " x " << PrimVertexX[l] << " +/- " << PrimVerSigX[l]
	   << " y " << PrimVertexY[l] << " +/- " << PrimVerSigY[l]
	   << " z " << PrimVertexZ[l] << " +/- " << PrimVerSigZ[l] << endl;
#endif
      RefMultZP->Fill(PrimVertexZ[l],PrimaryVertices_mRefMultPos[l]);
      RefMultZN->Fill(PrimVertexZ[l],PrimaryVertices_mRefMultNeg[l]);
      if (TMath::Abs(PrimVertexZ[l]) > zcut ) continue;
      Double_t vtx[3] = {PrimVertexX[l], PrimVertexY[l], PrimVertexZ[l]};
      DCAxy->Reset();
      DCAz->Reset();
      DCA->Reset();
      static const Double_t L = 34.42;  // half a SSD ladder's length 
      static const Double_t R = 22.80;
      Double_t eta_max = - TMath::Log(TMath::Tan(0.5*TMath::ATan2(R, L-PrimVertexZ[l])));
      Double_t eta_min = - TMath::Log(TMath::Tan(0.5*TMath::ATan2(R,-L-PrimVertexZ[l])));
      //      if (TMath::Abs(PrimVertexZ[l]) <= 25. ) continue;
      Int_t noGoodPrimaryTracks = 0;
      for (Int_t k = 0; k<NoTracks; k++) {
	//	if(NoTracks <= 10) break;            // more than 10 tracks please
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	if (mNHitsFit[k] < 15) continue;
	if (pT[k] < 0.1) continue;
	if (Eta[k] <= eta_min || Eta[k] >= eta_max) continue;
	noGoodPrimaryTracks++;
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
	Double_t P = sqrt(pZ[k]*pZ[k]+pT[k]*pT[k]);
	Int_t N = 0;
	N = NoFSvtSsdHits;
	Int_t kg = mIndex2Global[k];
	StThreeVectorF primP(pX[k],pY[k],pZ[k]);
#ifdef DEBUG
	cout << "Primary track k = " << k << " Global track kg = " << kg << endl;
	cout << "primary track momentum " << primP << endl;
// 	StThreeVectorF dcaP(mDCAGlobal_mX1[k],mDCAGlobal_mX2[k],mDCAGlobal_mX3[k]);
// 	StThreeVectorF dcaPG(mDCAGlobal_mX1[k],mDCAGlobal_mX2[k],mDCAGlobal_mX3[k]);
// 	cout << "dcaP: " << dcaP << "\t dcaPG " << dcaPG << endl;
#endif
	if (kg < 0 || kg > NoTracksGl) continue;
	StThreeVectorF globP;
	globP = StThreeVectorF(pXGl[kg],pYGl[kg],pZGl[kg]);
// 	StThreeVectorF dcaGG(mDCAGlobal_mX1Gl[kg],mDCAGlobal_mX2Gl[kg],mDCAGlobal_mX3Gl[kg]);
	StThreeVectorF dcaG(mDCAGlobal_mX1Gl[kg],mDCAGlobal_mX2Gl[kg],mDCAGlobal_mX3Gl[kg]);
#ifdef DEBUG
	cout << "global track momentum " << globP
	     << "\tdcaG: " << dcaG << endl;
#endif
#ifdef 	__OLD_DCA__
	StThreeVectorF dirG = globP.unit();
	Double_t cosL = dirG.perp();
	Double_t tanL = TMath::Tan(TMath::ACos(cosL));
	Double_t cosP = dirG.x()/cosL;
	Double_t sinP = dirG.y()/cosL;
	Double_t dcaXY = sinP * dcaG.x() - cosP * dcaG.y(); // switched sign
	Double_t dcaZ  = dcaG.z()/(cosL*cosL);
	Double_t sigmaXY   = mSigmaDcaDGl[kg];
	Double_t sigmaZ    = mSigmaDcaZGl[kg]/(cosL*cosL); // account bug in StMuTrack
#else
	Int_t kgc = GlobalTracks_mIndex2Cov[kg];
        static StDcaGeometry dcaGeometry;
	Float_t pars[6] = {
	  CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	  CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
	Float_t errs[15] = {
	  CovGlobTrack_mImpImp[kgc],
	  CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	  CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	  CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	  CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],CovGlobTrack_mTanTan[kgc]};
	dcaGeometry.set(pars, errs);
        THelixTrack     thelix =  dcaGeometry.thelix();
        thelix.Move(thelix.Path(vtx));
	Double_t dcaXY = dcaGeometry.impact();
	Double_t dcaZ  = dcaGeometry.z();
	Double_t dcaEmx[3];
	thelix.Dca(vtx,dcaXY,dcaZ,dcaEmx,2);
	Double_t sigmaXY   = 0;
	Double_t sigmaZ    = 0;
        if (dcaEmx[0] > 0) sigmaXY = TMath::Sqrt(dcaEmx[0]);
        if (dcaEmx[2] > 0) sigmaZ  = TMath::Sqrt(dcaEmx[2]);
#endif
	if (sigmaXY <= 0 || sigmaZ > 1 || sigmaZ <= 0 || sigmaZ > 1) {
#ifdef DEBUG
	  cout << "First Point x/y/z = " 
	       << GlobalTracks_mFirstPoint_mX1[kg] << "/" << GlobalTracks_mFirstPoint_mX2[kg] << "/" << GlobalTracks_mFirstPoint_mX3[kg] << endl;
	  cout << "Last Point x/y/z = " 
	       << GlobalTracks_mLastPoint_mX1[kg] << "/" << GlobalTracks_mLastPoint_mX2[kg] << "/" << GlobalTracks_mLastPoint_mX3[kg] << endl;
#endif
	  continue;
	}
	Double_t sigmaXYVx = TMath::Sqrt(sigmaXY* sigmaXY+ 
					 PrimVerSigX[l]*PrimVerSigX[l] +  
					 PrimVerSigY[l]*PrimVerSigY[l]);
	Double_t sigmaZVx  = TMath::Sqrt(sigmaZ*sigmaZ + PrimVerSigZ[l]*PrimVerSigZ[l]);
#ifdef DEBUG
	cout << "Sigma DCA xy " << sigmaXY << " => " << sigmaXYVx << endl;
	cout << "Sigma DCA z  " << sigmaZ  << " => " << sigmaZVx << endl;
#endif
#ifdef DEBUG
	cout << "dcaXY: " << dcaXY << "\t dcaZ " << dcaZ << endl;
#endif
	Double_t pullXY = dcaXY/sigmaXY;
	Double_t pullZ  = dcaZ /sigmaZ;
	Double_t pullXYVx = dcaXY/sigmaXYVx;
	Double_t pullZVx  = dcaZ /sigmaZVx;
	Double_t pull = TMath::Sqrt(pullXY*pullXY + pullZ*pullZ);
	if (N > 4) N = 4;
	dcaXYInvpT[N]->Fill(1./pTGl[kg],dcaXY);
	dcaXYInvp[N]->Fill(1./P,dcaXY);
	pullXYInvpT[N]->Fill(1./pTGl[kg],pullXY);
	pullXYInvpTVx[N]->Fill(1./pTGl[kg],pullXYVx);
	pullXYInvp[N]->Fill(1./P,pullXY);
	pullXYInvpVx[N]->Fill(1./P,pullXYVx);
	dcaZInvpT[N]->Fill(1./pTGl[kg],dcaZ);
	dcaZInvp[N]->Fill(1./P,dcaZ);
	pullZInvpT[N]->Fill(1./pTGl[kg],pullZ);
	pullZInvpTVx[N]->Fill(1./pTGl[kg],pullZVx);
	pullZInvp[N]->Fill(1./P,pullZ);
	pullZInvpVx[N]->Fill(1./P,pullZVx);
	if (pTGl[kg] > 1.) {
	  dcaXYPhi[N]->Fill(phi,dcaXY);
	  pullXYPhi[N]->Fill(phi,pullXY);
	  dcaZPhi[N]->Fill(phi,dcaZ);
	  pullZPhi[N]->Fill(phi,pullZ);
	}
	if (N >= 2) {
	  DCAxy->Fill(TMath::Abs(pullXY));
	  DCAz->Fill(TMath::Abs(pullZ));
	  DCA->Fill(pull);
	}
      }
#ifdef GLOBAL
      break;
#endif
      if (DCA->GetEntries() == 0) continue;
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
	}
      }
      if (noGoodPrimaryTracks) {
	VxSigmaX->Fill(TMath::Log10(noGoodPrimaryTracks),PrimVerSigX[l]);
	VxSigmaY->Fill(TMath::Log10(noGoodPrimaryTracks),PrimVerSigY[l]);
	VxSigmaZ->Fill(TMath::Log10(noGoodPrimaryTracks),PrimVerSigZ[l]);
      }
    }
  }
  fOut->Write();
  delete fOut;
}
