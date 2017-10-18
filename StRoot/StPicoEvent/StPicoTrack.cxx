#include <limits>

#include "TMath.h"

#include "St_base/StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StPicoEvent/StPicoTrack.h"
#include "StBichsel/StdEdxPull.h"


//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack() : TObject(),
			     mId(0),
			     mChi2(1000),
			     mPMomentum{}, 
			     mDedx(0.), mDedxError(0.), mDnDx(0.), mDnDxError(0.), mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0), mCharge(0),
			     mTopologyMap{}, mBEmcPidTraitsIndex(-1), mBTofPidTraitsIndex(-1), mMtdPidTraitsIndex(-1) 
{
}
//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack(StMuTrack const* const gTrk, StMuTrack const* const pTrk, double const B, StThreeVectorD const& pVtx, StDcaGeometry const& dcaG)
  : StPicoTrack()
{
  if (!gTrk || gTrk->type() != global || (pTrk && (pTrk->type() != primary || pTrk->id() != gTrk->id())))
  {
    LOG_WARN << "Invalid arguments passed to StPicoTrack constructor. Object is default initialized" << endm;

    return;
  }

  mId   = (UShort_t)gTrk->id();
  mChi2 = (gTrk->chi2()  > 1000) ? 1000 : gTrk->chi2();

  if (pTrk)
  {
    mPMomentum[0] = pTrk->p().x();
    mPMomentum[1] = pTrk->p().y();
    mPMomentum[2] = pTrk->p().z();
  }
  // Store dca
  const Float_t* params = dcaG.params();
  const Float_t* errMatrix = dcaG.errMatrix();
  //  dcaG.Print("");
  for (int i = 0; i < 6; i++) mPar[i] = params[i];
  /*                                                          j    0     1     2     3     4
    Float_t  mImpImp;                                       i 0  0(0) 
    Float_t  mZImp,   mZZ;                                    1  1(0)  2(1)
    Float_t  mPsiImp, mPsiZ, mPsiPsi;                         2  3(1)  4(2)  5(2)
    Float_t  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;                3  6(3)  7(4)  8(5)  9(3)
    Float_t  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;       4 10(6) 11(7) 12(8) 13(9) 14(4) 
   */
  Int_t ii = 0;
  for (int i = 0; i < 5; i++) {
    mSigma[i] = TMath::Sqrt(errMatrix[ii]);
    for (int j = 0; j < i; j++) {
      Int_t ij = ii - i - 1 + j + 1;
      Int_t ij1 = ij - i;
      mCorr[ij1] = errMatrix[ij]/(mSigma[i]*mSigma[j]);
    }
    ii += i+2;
  }
  //  StDcaGeometry b = dcaGeometry();
  //  b.Print("");
  //  for (int i = 0; i < 15; i++) mErrMatrix[i] = errMatrix[i];
  

  mDedx      = gTrk->dEdx() * 1.e6;
  mDedxError = gTrk->probPidTraits().dEdxErrorFit();
  mDnDx      = gTrk->probPidTraits().dNdxFit();
  mDnDxError = gTrk->probPidTraits().dNdxErrorFit();
  int flag = gTrk->flag();
  if (flag / 100 < 7) // TPC tracks
  {
    mNHitsFit  = (Char_t)(gTrk->nHitsFit(kTpcId));
    mNHitsMax  = (UChar_t)(gTrk->nHitsPoss(kTpcId));
  }
  else     // FTPC tracks
  {
    if (gTrk->helix().momentum(B * kilogauss).pseudoRapidity() > 0.)
      {
      mNHitsFit  = (Char_t)(gTrk->nHitsFit(kFtpcWestId));
      mNHitsMax  = (UChar_t)(gTrk->nHitsPoss(kFtpcWestId));
    }
    else
    {
      mNHitsFit  = (Char_t)(gTrk->nHitsFit(kFtpcEastId));
      mNHitsMax  = (UChar_t)(gTrk->nHitsPoss(kFtpcEastId));
    }
  }
  mNHitsDedx = (Char_t)(gTrk->nHitsDedx());
  mCharge    = (Char_t)(gTrk->charge());

  mTopologyMap[0] = (UInt_t)(gTrk->topologyMap().data(0));
  mTopologyMap[1] = (UInt_t)(gTrk->topologyMap().data(1));
}
//----------------------------------------------------------------------------------
void StPicoTrack::Print(Char_t const* option) const
{
  LOG_INFO << "id: " << id()
           << " chi2: " << chi2() << "\n"
           << "pMom: " << pMom()
           << " nHitsFit: " << nHitsFit()
           << " nHitsdEdx: " << nHitsDedx() << "\n"
           << "nSigma pi/K/p/e: " << nSigmaPion()   << "/" << nSigmaKaon() << "/"
           << nSigmaProton() << "/" << nSigmaElectron()
           << endm;
}
//________________________________________________________________________________
StDcaGeometry StPicoTrack::dcaGeometry() const {
  StDcaGeometry a;
  Float_t errMatrix[15];
  Int_t ii = 0;
  for (int i = 0; i < 5; i++) {
    errMatrix[ii] = mSigma[i]*mSigma[i];
    for (int j = 0; j < i; j++) {
      Int_t ij = ii - i - 1 + j + 1;
      Int_t ij1 = ij - i;
      errMatrix[ij] = mCorr[ij1]*mSigma[i]*mSigma[j];
    }
    ii += i+2;
  }
  a.set(mPar, errMatrix);
  return a;
}      
//________________________________________________________________________________
Float_t StPicoTrack::dEdxPull(Float_t mass, UChar_t fit, Int_t charge) const {
  Float_t z = -999.;
  Float_t momentum  = gMom().mag();
  Float_t betagamma = momentum*TMath::Abs(charge)/mass;
  Float_t dedx_measured, dedx_resolution = -1;
  if (! fit) { // I70
    dedx_measured = 1e-6*dEdx();
    dedx_resolution = dEdxError();
  } else if ( fit == 1) {     // Ifit
    dedx_measured = 1e-6*dEdx();
    dedx_resolution = dEdxError();
  } else {     // dNdx
    dedx_measured = dNdx();
    dedx_resolution = dNdxError();
  }
  if (dedx_resolution <= 0) return z;
  z = StdEdxPull::Eval(dedx_measured,dedx_resolution,betagamma,fit,1,charge);
  return z;
}
//________________________________________________________________________________
