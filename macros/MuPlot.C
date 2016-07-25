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
void MuPlot(const Char_t *files = "/star/u/kapitan/hipt/simu/tpc.svt.sdt/event/hijing*.MuDst.root"){
  TDirIter Dir(files);
  TTreeIter iter("MuDst");
  Char_t *file = 0;
  Int_t NFiles = 0;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  const Int_t&       NoPrimaryVertices                         = iter("PrimaryVertices");
  const Float_t*&    PrimaryVertices_mPosition_mX1            = iter("PrimaryVertices.mPosition.mX1");
  const Float_t*&    PrimaryVertices_mPosition_mX2            = iter("PrimaryVertices.mPosition.mX2");
  const Float_t*&    PrimaryVertices_mPosition_mX3            = iter("PrimaryVertices.mPosition.mX3");
  const Float_t*&    PrimaryVertices_mPosError_mX1            = iter("PrimaryVertices.mPosError.mX1");
  const Float_t*&    PrimaryVertices_mPosError_mX2            = iter("PrimaryVertices.mPosError.mX2");
  const Float_t*&    PrimaryVertices_mPosError_mX3            = iter("PrimaryVertices.mPosError.mX3");
  const Float_t*&    PrimaryVertices_mRanking                 = iter("PrimaryVertices.mRanking");
  const UShort_t*&   PrimaryVertices_mNTracksUsed             = iter("PrimaryVertices.mNTracksUsed");
  const Int_t&       NoGlobalTracks                           = iter("GlobalTracks");
  const Int_t&       NoTracks                   = iter("PrimaryTracks");
  const Int_t   *&PrimaryTracks_mVertexIndex = iter("PrimaryTracks.mVertexIndex");
  const Float_t*&    GlobalTracks_mDCA_mX1                    = iter("GlobalTracks.mDCA.mX1");
  const Float_t*&    GlobalTracks_mDCA_mX2                    = iter("GlobalTracks.mDCA.mX2");
  const Float_t*&    GlobalTracks_mDCA_mX3                    = iter("GlobalTracks.mDCA.mX3");
  const Float_t*&    PrimaryTracks_mDCAGlobal_mX1                    = iter("PrimaryTracks.mDCAGlobal.mX1");
  const Float_t*&    PrimaryTracks_mDCAGlobal_mX2                    = iter("PrimaryTracks.mDCAGlobal.mX2");
  const Float_t*&    PrimaryTracks_mDCAGlobal_mX3                    = iter("PrimaryTracks.mDCAGlobal.mX3");
  const UShort_t*&   PrimaryVertices_mRefMultNeg              = iter("PrimaryVertices.mRefMultNeg");
  const UShort_t*&   PrimaryVertices_mRefMultPos              = iter("PrimaryVertices.mRefMultPos");
  const Float_t*&    PrimaryVertices_mChiSquared              = iter("PrimaryVertices.mChiSquared");
  const Float_t*&    PrimaryTracks_mChiSqXY                   = iter("PrimaryTracks.mChiSqXY");
  const Float_t*&    PrimaryTracks_mChiSqZ                    = iter("PrimaryTracks.mChiSqZ");

  TH1D *NoPrimVerticesF   = new TH1D("NoPrimaryVertices","NoPrimaryVertices",100,0,100);
  TH2D *PrimaryVerticesXY = new TH2D("PrimaryVerticesXY","Primary Vertices XY",100,-5,5,100,-5,5);
  TH1D *PrimaryVerticesZ  = new TH1D("PrimaryVerticesZ","Primary Vertices Z",100,-200,200);
  TProfile *PrimErrXvsZ   = new TProfile("PrimErrXvsZ","Primary #sigma_{X} vs Z",100,-200,200);
  TProfile *PrimErrYvsZ   = new TProfile("PrimErrYvsZ","Primary #sigma_{Y} vs Z",100,-200,200);
  TProfile *PrimErrZvsZ   = new TProfile("PrimErrZvsZ","Primary #sigma_{Z} vs Z",100,-200,200);
  TProfile *PrimaryVerticesChiSquared = new TProfile("PrimaryVerticesChiSquared","PrimaryVerticesChiSquared vs Z",100,-200,200);
  TProfile *RefMultNeg    = new TProfile("RefMultNeg","RefMultNeg vs Z",100,-200,200);
  TProfile *RefMultPos    = new TProfile("RefMultPos","RefMultPos vs Z",100,-200,200);
  TProfile *NTracksUsed   = new TProfile("NTracksUsed","NTracksUsed vs Z",100,-200,200);

  TProfile *PrimTrkChisqXYvsZ= new TProfile("PrimTrkChisqXYvsZ","Primary #chi^2_{XY} vs Z",100,-200,200);
  TProfile *PrimTrkChisqZvsZ = new TProfile("PrimTrkChisqZvsZ","Primary #chi^2_{Z} vs Z",100,-200,200);
  TProfile *PrimTrkDcaX  = new TProfile("PrimTrkDcaX","PrimTrkDcaX vs Z",100,-200,200);
  TProfile *PrimTrkDcaY  = new TProfile("PrimTrkDcaY","PrimTrkDcaY vs Z",100,-200,200);
  TProfile *PrimTrkDcaZ  = new TProfile("PrimTrkDcaZ","PrimTrkDcaZ vs Z",100,-200,200);
  while (iter.Next()) {
    NoPrimVerticesF->Fill(NoPrimaryVertices);
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      PrimaryVerticesXY->Fill(PrimaryVertices_mPosition_mX1[l],PrimaryVertices_mPosition_mX2[l]);
      PrimaryVerticesZ->Fill(PrimaryVertices_mPosition_mX3[l]);
      PrimErrXvsZ->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mPosError_mX1[l]);
      PrimErrYvsZ->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mPosError_mX2[l]);
      PrimErrZvsZ->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mPosError_mX3[l]);
      PrimaryVerticesChiSquared->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mChiSquared[l]);
      RefMultNeg->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mRefMultNeg[l]);
      RefMultPos->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mRefMultPos[l]);
      NTracksUsed->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryVertices_mNTracksUsed[l]);
      for (Int_t k = 0; k < NoTracks; k++) {
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	PrimTrkChisqXYvsZ->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryTracks_mChiSqXY[k]);
	PrimTrkChisqZvsZ->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryTracks_mChiSqZ[k]);
	PrimTrkDcaX->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryTracks_mDCAGlobal_mX1[k]);
	PrimTrkDcaY->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryTracks_mDCAGlobal_mX2[k]);
	PrimTrkDcaZ->Fill(PrimaryVertices_mPosition_mX3[l],PrimaryTracks_mDCAGlobal_mX3[k]);
      }
    }
  }
}
