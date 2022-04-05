/* 
   Reconstruction of primary vertices from MuDst.
   In directory where you have *MuDst.root files run
   root.exe lMuDstK.C MuPrmVtx.C+
   $Id: MuPrmVtx.C,v 2.3 2013/04/10 22:14:20 fisyak Exp $
 */
#define DEBUG
#define __IDTRUTH__

#define DRAW_RECO_HISTOS // draw histos which are used for reconstruction
//#define EVENT_DISPLAY // show tracks and vertices in each event on the screen

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
#include "StDcaGeometry.h"
#include "KFVertex.h"
#include "MTrack.h"
#include "VVertex.h"
#include "TH1K.h"
#include "TSpectrum.h"
#include "TVirtualFitter.h"
#include "TPolyMarker.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "Math/Functor.h"
#include "Math/GSLMinimizer1D.h"
#include "TROOT.h"
#include "TVector3.h"
#include "TArrayF.h"
#include "TClonesArray.h"
#include "StKFVertex/StAnneling.h"
#include "StKFVertex/StKFEvent.h"
#include "StKFVertex/StKFTrack.h"
#include "StKFVertex/StKFVertex.h"
#include "StKFVertex/StKFVerticesCollection.h"
#include "StKFVertex/StMuDstVtxT.h"
#include "StKFVertex/StVertexP.h"
#include "StKFVertex/StVertexT.h"
#include "StKFVertex/StKFVertexFinder.h"
#ifdef EVENT_DISPLAY
#include "StDraw3D.h"
static StDraw3D *eventD = 0;
#endif
#endif
#if 0
static Int_t beamLine = 0;
static KFParticle beam;
static Double_t pZ = 1000;
#endif
Bool_t Ask() {
  static Bool_t fAsk = kTRUE;
  char symbol;
  if (fAsk){
    std::cout << "ask (enter - next, s - save, r - don't ask, q - quit) >";
    do{
      std::cin.get(symbol);
      if (symbol == 'r')
        fAsk = kFALSE;
      if (symbol == 'q')
        return kTRUE;
    } while (symbol != '\n');
    std::cout << endl;
  }
  return kFALSE;
}
//________________________________________________________________________________
void MuPrmVtx(Int_t first, Int_t last, const Char_t *files, const Char_t* Out="") { 
  TDirIter Dir(files);
  TString Files(files);
#if 0
  //  if (Files.Contains("pp500") && ! Files.Contains("sim")) beamLine = 1;
  /* beam line
     SELECT * FROM Calibrations_rhic.vertexSeed v where beginTime > "2009-03-01" and beginTime < "2009-04-01";
     |  beginTime           | x0         | dxdz       | y0         | dydz        | err_x0     | err_dxdz   | err_y0     | err_dydz   
     +----------------------+------------+------------+------------+-------------+------------+------------+------------+------------
     |  2009-03-26 04:10:21 | 0.42600000 | 0.00118000 | 0.00800000 |  0.00024000 | 0.00900000 | 0.00003000 | 0.00900000 | 0.00003000  
     |  2009-03-26 21:40:24 | 0.42600000 | 0.00136000 | 0.01600000 | -0.00005000 | 0.00900000 | 0.00003000 | 0.00900000 | 0.00003000  
     |  2010-03-20 01:00:00 | 0.27000001 | 0.00000000 |-0.03000000 |  0.00000000 | 0.15000001 | 0.00000000 | 0.15000001 | 0.00000000 
     |  2010-04-09 07:15:15 | 0.27664959 | 0.00127356 |-0.07965557 | -0.00001392 | 0.00289759 | 0.00007723 | 0.00288955 | 0.00008473 
  */
  struct vertexSeed_t {Double_t x0,       dxdz,         y0,        dydz,     err_x0,   err_dxdz,     err_y0,   err_dydz;};
  vertexSeed_t b    = { 0.42600000, 0.00118000, 0.00800000,  0.00024000, 0.00900000, 0.00003000, 0.00900000, 0.00003000};
#endif
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  if (! NFiles) return;
  TString output(Out);
  if (output == "") {
    if (! iter.Chain()) return;
    if (! iter.Chain()->GetListOfFiles()) return;
    const Char_t *file1 = iter.Chain()->GetListOfFiles()->At(0)->GetTitle();
    TString dir = gSystem->BaseName(file1); 
    dir.ReplaceAll(".MuDst","");
    output += dir;
  }
  TFile *fOut = new TFile(output,"recreate");
  TTree *ftree = new TTree("StVertexT","Vertex tree");
  ftree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  Int_t bufsize = 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
  StKFEvent *fStKFEvent = new StKFEvent();
  TTree::SetBranchStyle(1); //new style by default
  TBranch *branch = ftree->Branch("StKFEvent", "StKFEvent", &fStKFEvent, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  
  const Int_t*&      MuEvent_mEventInfo_mRunId                = iter("MuEvent.mEventInfo.mRunId");
  const Int_t*&      MuEvent_mEventInfo_mId                   = iter("MuEvent.mEventInfo.mId");
  const Int_t&       NoPrimaryVertices                        = iter("PrimaryVertices");
  //  const Float_t*&    MuEvent_mRunInfo_mTpcDriftVelocity       = iter("MuEvent.mRunInfo.mTpcDriftVelocity[2]");
  const Float_t*&    PrimaryVertices_mPosition_mX1            = iter("PrimaryVertices.mPosition.mX1");
  const Float_t*&    PrimaryVertices_mPosition_mX2            = iter("PrimaryVertices.mPosition.mX2");
  const Float_t*&    PrimaryVertices_mPosition_mX3            = iter("PrimaryVertices.mPosition.mX3");
  const Float_t*&    PrimaryVertices_mPosError_mX1            = iter("PrimaryVertices.mPosError.mX1");
  const Float_t*&    PrimaryVertices_mPosError_mX2            = iter("PrimaryVertices.mPosError.mX2");
  const Float_t*&    PrimaryVertices_mPosError_mX3            = iter("PrimaryVertices.mPosError.mX3");
  const Float_t*&    PrimaryVertices_mRanking                 = iter("PrimaryVertices.mRanking");
  const UShort_t*&   PrimaryVertices_mNTracksUsed             = iter("PrimaryVertices.mNTracksUsed");
#ifdef  __IDTRUTH__
  const UShort_t*&   PrimaryVertices_mIdTruth                 = iter("PrimaryVertices.mIdTruth");
  const UShort_t*&   PrimaryVertices_mQuality                 = iter("PrimaryVertices.mQuality");
  const Int_t*&      PrimaryVertices_mIdParent                  = iter("PrimaryVertices.mIdParent"); // >>
#endif  
  const Int_t*&      PrimaryTracks_mVertexIndex               = iter("PrimaryTracks.mVertexIndex");
  const Int_t& NoPrimaryTracks = iter("PrimaryTracks");
  const Int_t*& PrimaryTracks_mIndex2Global = iter("PrimaryTracks.mIndex2Global");
  const Float_t*&    PrimaryTracks_mChiSqZ                    = iter("PrimaryTracks.mChiSqZ");
#ifdef  __IDTRUTH__0
  const UShort_t*&   PrimaryTracks_mIdTruth                   = iter("PrimaryTracks.mIdTruth");
  const UShort_t*&   PrimaryTracks_mQuality                   = iter("PrimaryTracks.mQuality");
  const Int_t*&      PrimaryTracks_mIdParentVx                = iter("PrimaryTracks.mIdParentVx");
#endif
  const Int_t& NoGlobalTracks = iter("GlobalTracks");
  const Short_t*& GlobalTracks_mFlag = iter("GlobalTracks.mFlag");
  const Float_t*&    GlobalTracks_mEta                        = iter("GlobalTracks.mEta");
  const Float_t*&    GlobalTracks_mFirstPoint_mX3             = iter("GlobalTracks.mFirstPoint.mX3");
#ifdef  __IDTRUTH__
  const UShort_t*&   GlobalTracks_mIdTruth                    = iter("GlobalTracks.mIdTruth");
  const UShort_t*&   GlobalTracks_mQuality                    = iter("GlobalTracks.mQuality");
  const Int_t*&      GlobalTracks_mIdParentVx                 = iter("GlobalTracks.mIdParentVx");
#endif
  //  const UChar_t*&    GlobalTracks_mNHitsFit                   = iter("GlobalTracks.mNHitsFit");
  //  const Float_t*&    GlobalTracks_mChiSqXY                    = iter("GlobalTracks.mChiSqXY");
  const Int_t*& GlobalTracks_mIndex2Cov = iter("GlobalTracks.mIndex2Cov");
  const Int_t& NoCovGlobTrack = iter("CovGlobTrack");
  const Float_t*& CovGlobTrack_mImp = iter("CovGlobTrack.mImp");
  const Float_t*& CovGlobTrack_mZ = iter("CovGlobTrack.mZ");
  const Float_t*& CovGlobTrack_mPsi = iter("CovGlobTrack.mPsi");
  const Float_t*& CovGlobTrack_mPti = iter("CovGlobTrack.mPti");
  const Float_t*& CovGlobTrack_mTan = iter("CovGlobTrack.mTan");
  const Float_t*& CovGlobTrack_mCurv = iter("CovGlobTrack.mCurv");
  const Float_t*& CovGlobTrack_mImpImp = iter("CovGlobTrack.mImpImp");
  const Float_t*& CovGlobTrack_mZImp = iter("CovGlobTrack.mZImp");
  const Float_t*& CovGlobTrack_mZZ = iter("CovGlobTrack.mZZ");
  const Float_t*& CovGlobTrack_mPsiImp = iter("CovGlobTrack.mPsiImp");
  const Float_t*& CovGlobTrack_mPsiZ = iter("CovGlobTrack.mPsiZ");
  const Float_t*& CovGlobTrack_mPsiPsi = iter("CovGlobTrack.mPsiPsi");
  const Float_t*& CovGlobTrack_mPtiImp = iter("CovGlobTrack.mPtiImp");
  const Float_t*& CovGlobTrack_mPtiZ = iter("CovGlobTrack.mPtiZ");
  const Float_t*& CovGlobTrack_mPtiPsi = iter("CovGlobTrack.mPtiPsi");
  const Float_t*& CovGlobTrack_mPtiPti = iter("CovGlobTrack.mPtiPti");
  const Float_t*& CovGlobTrack_mTanImp = iter("CovGlobTrack.mTanImp");
  const Float_t*& CovGlobTrack_mTanZ = iter("CovGlobTrack.mTanZ");
  const Float_t*& CovGlobTrack_mTanPsi = iter("CovGlobTrack.mTanPsi");
  const Float_t*& CovGlobTrack_mTanPti = iter("CovGlobTrack.mTanPti");
  const Float_t*& CovGlobTrack_mTanTan = iter("CovGlobTrack.mTanTan");
  //  const Float_t*& Event_mMagneticField = iter("Event.mMagneticField");
  const Double_t*&   Event_mMagneticField         = iter("MuEvent.mRunInfo.mMagneticFieldZ");
#ifdef  __IDTRUTH__
  const Int_t&       NoMuMcVertex                             = iter("StMuMcVertex");
#if 0
  const Int_t*&      StMuMcVertex_Id                          = iter("StMuMcVertex.mId");
#endif
  const Int_t*&      StMuMcVertex_NoDaughters                 = iter("StMuMcVertex.mNoDaughters");
  const Int_t*&      StMuMcVertex_IdParTrk                    = iter("StMuMcVertex.mIdParTrk");
  const Float_t*&    StMuMcVertex_time                        = iter("StMuMcVertex.mTime");
  const Float_t*&    StMuMcVertex_xyzV_mX1                    = iter("StMuMcVertex.mXyzV.mX1");
  const Float_t*&    StMuMcVertex_xyzV_mX2                    = iter("StMuMcVertex.mXyzV.mX2");
  const Float_t*&    StMuMcVertex_xyzV_mX3                    = iter("StMuMcVertex.mXyzV.mX3");
  const Int_t&       NoMuMcTrack                              = iter("StMuMcTrack");
#if 0
  const Int_t*&      StMuMcTrack_Id                           = iter("StMuMcTrack.mId");
#endif
  const Int_t*&      StMuMcTrack_gePid                        = iter("StMuMcTrack.mGePid");
#if 0
  const Int_t*&      StMuMcTrack_IdVx                         = iter("StMuMcTrack.mIdVx");
  const Int_t*&      StMuMcTrack_IdVxEnd                      = iter("StMuMcTrack.mIdVxEnd");
  const Float_t*&    StMuMcTrack_pxyz_mX1                     = iter("StMuMcTrack.mPxyz.mX1");
  const Float_t*&    StMuMcTrack_pxyz_mX2                     = iter("StMuMcTrack.mPxyz.mX2");
  const Float_t*&    StMuMcTrack_pxyz_mX3                     = iter("StMuMcTrack.mPxyz.mX3");
#endif
#endif
  StKFVertexFinder fitter;
#ifdef DRAW_RECO_HISTOS
  if (! gROOT->IsBatch()) {TCanvas *c1 = new TCanvas("c1z","c1z",1400,600); fitter.SetCanvas(c1);}
#endif // DRAW_RECO_HISTOS

  //         Now iterations
  Int_t nev = 0;
  while (iter.Next()) { // events loop
    nev++;
    if (nev < first) continue;
    if (nev > last) break;
#ifdef EVENT_DISPLAY
    if (! eventD) eventD = new StDraw3D(); // + Geometry  // StDraw3D *eventD = new StDraw3D(0);// View with no detector geometry decoration
    else          eventD->Clear();
#endif // EVENT_DISPLAY
    fStKFEvent->Clear();
    KFParticle::SetField(Event_mMagneticField[0]);
#ifdef DEBUG
    cout << "#" << nev << "\tRun " << MuEvent_mEventInfo_mRunId[0] << "\tEvent " << MuEvent_mEventInfo_mId[0] << endl;
    cout << "NoTracks\t" << (int) NoGlobalTracks << " global\t" <<  (int) NoPrimaryTracks << " primary" << endl;
#endif
    fitter.Reset();
#if 0
    TObjArray tracks;
    tracks.SetOwner(kTRUE);
    TObjArray particles;
    particles.SetOwner(kTRUE);
#endif
    // Add measured multiplicities
    Double_t ymax = fitter.Vtx()->GetMaximum();
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      Int_t mult = 0;
      Int_t multP = 0;
      Int_t mWest = 0;
      Int_t mEast = 0;
      Int_t Q     = 0;
      for (Int_t k = 0; k <NoPrimaryTracks; k++) {
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	Int_t kg = PrimaryTracks_mIndex2Global[k];
	if (kg < 0 ||   kg >= NoGlobalTracks) continue;
	if (GlobalTracks_mFlag[kg]     <   0) continue;     // Bad fit
	if (GlobalTracks_mFlag[kg]     > 700) continue;     // FTPC
	if (GlobalTracks_mFlag[kg]%100 == 11) continue;     // Short track pointing to EEMC
	Int_t kgc = GlobalTracks_mIndex2Cov[kg];
	if (kgc < 0 || kgc > NoCovGlobTrack) continue;
	mult++;
	if (CovGlobTrack_mPti[kgc] < 0) Q -= 1;
	else                            Q += 1;
	if (PrimaryTracks_mChiSqZ[k] <  StAnneling::Chi2Cut()) multP++;
	if (GlobalTracks_mEta[kg] > 0 &&  GlobalTracks_mFirstPoint_mX3[kg] > 0) mWest++;
	if (GlobalTracks_mEta[kg] < 0 &&  GlobalTracks_mFirstPoint_mX3[kg] < 0) mEast++;
      }
      StMuDstVtxT V(PrimaryVertices_mPosition_mX1[l],PrimaryVertices_mPosition_mX2[l],PrimaryVertices_mPosition_mX3[l],
		    PrimaryVertices_mPosError_mX1[l],PrimaryVertices_mPosError_mX2[l],PrimaryVertices_mPosError_mX3[l],
		    PrimaryVertices_mNTracksUsed[l],mult,multP,mWest,mEast,Q,PrimaryVertices_mRanking[l],
		    PrimaryVertices_mIdTruth[l],PrimaryVertices_mQuality[l],PrimaryVertices_mIdParent[l]);
#ifdef  __IDTRUTH__
      if (V.QaTruth() > 0) {
	V.SetMc(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
		StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
		StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
      }
#endif
      cout << Form("MuDst       Primary Vertex: %3i with ",l) << V << endl;
      fStKFEvent->AddMuVtx(V);
      Double_t X = PrimaryVertices_mPosition_mX3[l];
      Double_t Y = mult;
      if (1.1*Y > ymax) ymax = 1.1*Y;
      TPolyMarker * pm = new TPolyMarker(1, &X, &Y);
      fitter.VtxM()->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(20);
      pm->SetMarkerColor(l+2);
      pm->SetMarkerSize(2);
      Y = multP;
      pm = new TPolyMarker(1, &X, &Y);
      fitter.VtxM()->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(21);
      pm->SetMarkerColor(l+2);
      pm->SetMarkerSize(2);
    };
    fitter.Vtx()->SetMaximum(ymax);
    Int_t NGoodGlobals = 0;
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
#if 0
      tracks.AddAtAndExpand (0,kg);
      particles.AddAtAndExpand (0,kg);
#endif
      if (GlobalTracks_mFlag[kg]     <   0) continue;     // Bad fit
      if (GlobalTracks_mFlag[kg]     > 700) continue;     // FTPC
      if (GlobalTracks_mFlag[kg]%100 == 11) continue;     // Short track pointing to EEMC
      //      if (TMath::Abs(GlobalTracks_mEta[kg]) > 5) continue;
      Int_t kgc = GlobalTracks_mIndex2Cov[kg];
      if (kgc < 0 || kgc > NoCovGlobTrack) continue;
      //      if (TMath::Abs(CovGlobTrack_mImp[kgc]) >   10) continue;
      //      if (TMath::Abs(CovGlobTrack_mZ[kgc])   > zmax) continue;
      
      Double_t parsT[6] = {
	CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
      Double_t errsT[15] = {
	CovGlobTrack_mImpImp[kgc],
	CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],
	CovGlobTrack_mTanTan[kgc]};
      StDcaGeometry *dca = new StDcaGeometry();
      dca->set(parsT, errsT);
      KFParticle *particle = fitter.AddTrackAt(dca,kg);
      delete dca;
      Int_t iWE  = 0;
      if (GlobalTracks_mEta[kg] > 0 &&  GlobalTracks_mFirstPoint_mX3[kg] > 0) iWE = 1;
      if (GlobalTracks_mEta[kg] < 0 &&  GlobalTracks_mFirstPoint_mX3[kg] < 0) iWE = 2;
      particle->SetID(10000*iWE + kg+1);
      particle->SetIdTruth(GlobalTracks_mIdTruth[kg],GlobalTracks_mQuality[kg]);
      particle->SetIdParentVx(GlobalTracks_mIdParentVx[kg]);
#ifdef DEBUG2
      cout << "particle: " << *particle << endl;
#endif
      //      tracks.AddAt(dca,kg);
      NGoodGlobals++;
    }

    if (NGoodGlobals < 2) continue;
    fitter.Fit();
    if (! fitter.Vertices()) continue;
    fitter.Vertices()->SetMc(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
					    StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
					    StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
    fitter.Vertices()->Print();
    
    Int_t Nvtx = fitter.Vertices()->NoVertices();
    for (Int_t l = 0; l < Nvtx; l++) {
      StKFVertex *V = fitter.Vertices()->Vertex(l);
      if (V) {
        fStKFEvent->AddKFVtx(*V);
      }
    }

    
    // Matching Dst => KFVertex 3D chi2
    Int_t NoMuDstVtx = fStKFEvent->NoMuDstVtx();
    Int_t NoKFVtx    = fStKFEvent->NoKFVtx();
    for (Int_t i = 0; i < NoMuDstVtx; i++) {
      StVertexT *VI = (StVertexT *) (*(fStKFEvent->MuDstVtx()))[i];
      for (Int_t j = 0; j < NoKFVtx; j++) {
	StVertexT *VJ = (StVertexT *) (*(fStKFEvent->KFVtx()))[j];
	TVector3 diff = VI->Xyz() - VJ->Xyz();
	Double_t chi2 =
	  diff.x()*diff.x()/(VI->SigmaXyz().x()*VI->SigmaXyz().x() + VJ->SigmaXyz().x()*VJ->SigmaXyz().x()) +
	  diff.y()*diff.y()/(VI->SigmaXyz().y()*VI->SigmaXyz().y() + VJ->SigmaXyz().y()*VJ->SigmaXyz().y()) +
	  diff.z()*diff.z()/(VI->SigmaXyz().z()*VI->SigmaXyz().z() + VJ->SigmaXyz().z()*VJ->SigmaXyz().z());
	if (chi2 < 1e3) {
	  fStKFEvent->AddDKFPair(i,j,*VI,*VJ,chi2);
	}
      }
    }
    for (Int_t i = 1; i < NoKFVtx; i++) {
      StVertexT *VI = (StVertexT *) (*(fStKFEvent->KFVtx()))[i];
      for (Int_t j = 0; j < i; j++) {
	StVertexT *VJ = (StVertexT *) (*(fStKFEvent->KFVtx()))[j];
	TVector3 diff = VI->Xyz() - VJ->Xyz();
	Double_t chi2 =
	  diff.x()*diff.x()/(VI->SigmaXyz().x()*VI->SigmaXyz().x() + VJ->SigmaXyz().x()*VJ->SigmaXyz().x()) +
	  diff.y()*diff.y()/(VI->SigmaXyz().y()*VI->SigmaXyz().y() + VJ->SigmaXyz().y()*VJ->SigmaXyz().y());
	if (chi2 < 1e3) {
	  fStKFEvent->AddKFKFPair(i,j,*VI,*VJ,chi2);
	}
      }
    }
    ftree->Fill();
#ifdef EVENT_DISPLAY
      // fill
    TArrayF xyzMcVxT(3*NoMuMcVertex); // from trigger event
    TArrayF xyzMcVxP(3*NoMuMcVertex); // from pile up
    Int_t NT = 0;
    Int_t NP = 0;
    for( int i = 0; i < NoMuMcVertex; ++i ) {
      if (TMath::Abs(StMuMcVertex_time[i]) < 100e-9) {
	xyzMcVxT[3*NT  ] = StMuMcVertex_xyzV_mX1[i];
	xyzMcVxT[3*NT+1] = StMuMcVertex_xyzV_mX2[i];
	xyzMcVxT[3*NT+2] = StMuMcVertex_xyzV_mX3[i];
	NT++;
      } else {
	xyzMcVxP[3*NP  ] = StMuMcVertex_xyzV_mX1[i];
	xyzMcVxP[3*NP+1] = StMuMcVertex_xyzV_mX2[i];
	xyzMcVxP[3*NP+2] = StMuMcVertex_xyzV_mX3[i];
	NP++;
      }
    }
    TArrayF xyzRcVx(3*Nvtx);
    Int_t NR = 0;
    for (Int_t l = 0; l < Nvtx; l++) {
      StKFVertex *V = fitter.Vertices()->Vertex(l);
      if (V) {
	xyzRcVx[3*NR  ] = V->Vertex().GetX();
	xyzRcVx[3*NR+1] = V->Vertex().GetY();
	xyzRcVx[3*NR+2] = V->Vertex().GetZ();
	NR++;
      }
    }
    eventD->Points(NR, xyzRcVx.GetArray(), kVtx); eventD->SetComment("Rc Vtx and Geometry");
    eventD->Points(NT, xyzMcVxT.GetArray(),kUsedHit); eventD->SetComment("Mc Vtx triggered and Geometry");
    eventD->Points(NP, xyzMcVxP.GetArray(),kUnusedHit); eventD->SetComment("Mc Vtx pile-up and Geometry");
    eventD->UpdateModified(); 
    while(!gSystem->ProcessEvents()){};
#endif // EVENT_DISPLAY
#if defined(DRAW_RECO_HISTOS) || defined(EVENT_DISPLAY)
    if (! gROOT->IsBatch() && Ask()) break;
#endif 
  } // loop ove events

  fOut->Write();
}
//________________________________________________________________________________
void Analysis(const Char_t *files="./y*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  TFile *fOut = new TFile("MuDst_KFV.root","recreate");
  TH2D *multDK = new TH2D("multDK","log_{2} (Multiplicity_{MuDst}) versus log_{2} (Multiplicity_{KFVertex})",
			  120,-1.0,11.0, 120,-1.0,11.0);
  TH2D *multDKQ = new TH2D("multDKQ","log_{2} (MultiplicityQ_{MuDst}) versus log_{2} (MultiplicityQ_{KFVertex})",
			   120,-1.0,11.0, 120,-1.0,11.0);
  TH2D *dXD = new TH2D("dXD","dX MuDst - MC versus log_{2} (Multiplicity_{MuDst})",120,-1.0,11.0,100,-5,5);
  TH2D *dYD = new TH2D("dYD","dY MuDst - MC versus log_{2} (Multiplicity_{MuDst})",120,-1.0,11.0,100,-5,5);
  TH2D *dZD = new TH2D("dZD","dZ MuDst - MC versus log_{2} (Multiplicity_{MuDst})",120,-1.0,11.0,100,-5,5);
  
  TH2D *dXK = new TH2D("dXK","dX KFVertex - MC versus log_{2} (Multiplicity_{KFVertex})",120,-1.0,11.0,100,-5,5);
  TH2D *dYK = new TH2D("dYK","dY KFVertex - MC versus log_{2} (Multiplicity_{KFVertex})",120,-1.0,11.0,100,-5,5);
  TH2D *dZK = new TH2D("dZK","dZ KFVertex - MC versus log_{2} (Multiplicity_{KFVertex})",120,-1.0,11.0,100,-5,5);
  cout << "|Simulation Production | Total no. | MuDst Efficiency | <Multiplicity> | KFVertex Efficiency | <Multiplicity>|" << endl;
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("event") || File.Contains("geant") ||
	File.Contains("hist")  || File.Contains("tags") ||  File.Contains("runco") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("MuDst")) continue;
    TFile *f = new TFile (File);
    if (! f) continue;
    TTree *tree = (TTree *) f->Get("StVertexT");
    if (! tree ) {delete f; continue;}
    TString Name(gSystem->BaseName(f->GetName()));
    Name.ReplaceAll(".root","");
    TString Title(Name);
    Title.ReplaceAll("_"," ");
    fOut->cd();
 
    TH1D *DM  = new TH1D(Form("DM%s",Name.Data()),Form("MuDst Multiplicity for %s",Title.Data()),3000,0,3000);
    TH1D *DMQ = new TH1D(Form("DMQ%s",Name.Data()),Form("MuDst Multiplicity*QA for %s",Title.Data()),3000,0,3000);
    TH1D *KM  = new TH1D(Form("KM%s",Name.Data()),Form("KFVer Multiplicity for %s",Title.Data()),3000,0,3000);
    TH1D *KMQ = new TH1D(Form("KMQ%s",Name.Data()),Form("KFVer Multiplicity*QA for %s",Title.Data()),3000,0,3000);
    StKFEvent *fStKFEvent = new StKFEvent();
    TBranch *branch = tree->GetBranch("StKFEvent");
    if (! branch) continue;
    //    tree->SetMakeClass(1);
    branch->SetAddress(&fStKFEvent);
    Int_t nbytes = 0, nb = 0;// ierr = 0, nevt = 0;
    Long64_t nentries = tree->GetEntries();
    for (Long64_t jentry=0; jentry<nentries;jentry++) {
      //in case of a TTree, ientry is the entry number in the current file
      if (tree->LoadTree(jentry) < 0) break;
      nb = tree->GetEntry(jentry);   nbytes += nb;
      Int_t NoMuDst = fStKFEvent->NoMuDstVtx();
      Int_t NoKFVtx = fStKFEvent->NoKFVtx();
      //      cout << "Event. \t" << jentry << "\tNo Dst " << NoMuDst << "\tKFV " << NoKFVtx << endl;
      // find IdTruth = 1 vertices and plot them
      TClonesArray *MuDst = fStKFEvent->MuDstVtx();
      Int_t MultMx = -1;
      Int_t kd = -1;
      Double_t MultD = 0.5;
      Double_t MultDQ = 0.5;
      TVector3 dXyz;
      for (Int_t l = 0; l < NoMuDst; l++) {
	StMuDstVtxT *md = (StMuDstVtxT *) MuDst->UncheckedAt(l);
	if (md->IdTruth() != 1) continue;
	Int_t Mult = md->Mult();
	if (Mult > MultMx) {MultMx = Mult; kd = l;}
      }
      if (kd >= 0) {
	StMuDstVtxT *md = (StMuDstVtxT *) MuDst->UncheckedAt(kd);
	MultD = md->Mult();
	DM->Fill(MultD);
	MultDQ = TMath::Nint(MultD*md->QaTruth()/100.);
	DMQ->Fill(MultDQ);
	dXyz = md->Xyz() - md->XyzMc();
	dXD->Fill(TMath::Log2(MultD),dXyz.x());
	dYD->Fill(TMath::Log2(MultD),dXyz.y());
	dZD->Fill(TMath::Log2(MultD),dXyz.z());
      }
      TClonesArray *KF = fStKFEvent->KFVtx();
      MultMx = -1;
      Int_t kf = -1;
      for (Int_t l = 0; l < NoKFVtx; l++) {
	StVertexT *mk = (StVertexT *) KF->UncheckedAt(l);
	if (mk->IdTruth() != 1) continue;
	Int_t Mult = mk->Mult();
	if (Mult > MultMx) {MultMx = Mult; kf = l;}
      }
      Double_t MultK = 0.5;
      Double_t MultKQ = 0.5;
      if (kf >= 0) {
	StVertexT *mk = (StVertexT *) KF->UncheckedAt(kf);
	MultK = mk->Mult();
	KM->Fill(MultK);
	MultKQ = TMath::Nint(MultK*mk->QaTruth()/100.);
	KMQ->Fill(MultKQ);
	dXyz = mk->Xyz() - mk->XyzMc();
	dXK->Fill(TMath::Log2(MultK),dXyz.x());
	dYK->Fill(TMath::Log2(MultK),dXyz.y());
	dZK->Fill(TMath::Log2(MultK),dXyz.z());
      }
      multDK->Fill(TMath::Log2(MultK), TMath::Log2(MultD));
      multDKQ->Fill(TMath::Log2(MultKQ), TMath::Log2(MultDQ));
    }
    cout << "Process \t" << f->GetName() << "\tread " << nentries << " entries and " <<  nbytes << " Bytes" << endl;
    delete f;
    TH1D *hists[4] = {DM, DMQ, KM, KMQ};
    cout << "|" << DM->GetTitle() << "|\t" << nentries;
    for (Int_t i = 0; i < 4; i++) {
#if 0
      cout << hists[i]->GetName() << "\t" << hists[i]->GetTitle()
	   << "\tEntries = " << hists[i]->GetEntries()
	   << "\tMean = " << hists[i]->GetMean() << endl;
#else
      cout << "|\t" << 100*hists[i]->GetEntries()/nentries << "\t|\t" << hists[i]->GetMean();
#endif
    }
    cout << "\t|" << endl;
    // compress multiplicity histograms
    TH1D *DMr = 0, *DMQr = 0, *KMr = 0, *KMQr = 0;
    Int_t nx = DM->GetNbinsX();
    for (Int_t i = nx; i > 0; i--) {
      if (! DMr) {
	if (DM->GetBinContent(i) <= 0 && KM->GetBinContent(i) <= 0) continue;
	DMr  = new TH1D(Form("DMr%s",Name.Data()),Form("MuDst Multiplicity for %s",Title.Data()),100,0,DM->GetXaxis()->GetBinUpEdge(i));
	DMQr = new TH1D(Form("DMQr%s",Name.Data()),Form("MuDst Multiplicity*QA for %s",Title.Data()),100,0,DM->GetXaxis()->GetBinUpEdge(i));
	KMr  = new TH1D(Form("KMr%s",Name.Data()),Form("KFVer Multiplicity for %s",Title.Data()),100,0,DM->GetXaxis()->GetBinUpEdge(i));
	KMQr = new TH1D(Form("KMQr%s",Name.Data()),Form("KFVer Multiplicity*QA for %s",Title.Data()),100,0,DM->GetXaxis()->GetBinUpEdge(i));
      }
      DMr->Fill(DM->GetBinCenter(i),DM->GetBinContent(i));
      DMQr->Fill(DMQ->GetBinCenter(i),DMQ->GetBinContent(i));
      KMr->Fill(KM->GetBinCenter(i),KM->GetBinContent(i));
      KMQr->Fill(KMQ->GetBinCenter(i),KMQ->GetBinContent(i));
    }
    delete DM; delete DMQ; delete KM; delete KMQ;
  }
  fOut->Write();
}
//________________________________________________________________________________
void MuPrmVtx(Int_t last, const Char_t *files = "./*MuDst.root", const Char_t *Out="") { 
  MuPrmVtx(0,last,files,Out);
}
//________________________________________________________________________________
void MuPrmVtx(const Char_t *files = "./*MuDst.root", const Char_t *Out="") { 
  MuPrmVtx(0,99999,files,Out);
}
//________________________________________________________________________________
// $Log: MuPrmVtx.C,v $
// Revision 2.3  2013/04/10 22:14:20  fisyak
// Roll back to version 04/04/2013
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.13  2012/02/18 23:20:52  fisyak
// Rename StKFVertexFitter => StKFVertexFinder
//
// Revision 1.12  2012/02/07 19:38:26  fisyak
// Repackage
//
