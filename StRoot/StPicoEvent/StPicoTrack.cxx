#include <limits>

#include "TMath.h"

#include "St_base/StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StPicoEvent/StPicoTrack.h"


//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack() : TObject(),
			     mId(0),
			     mChi2(1000),
			     mPMomentum{}, mGMomentum{}, mOrigin{},
			     mDedx(0.), mDnDx(0.), mDnDxError(0.), mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0), mCharge(0),
			     mNSigmaPion(1000),
			     mNSigmaKaon(1000),
			     mNSigmaProton(1000),
			     mNSigmaElectron(1000),
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
  
  // Calculate global momentum and position at point of DCA to the pVtx
  StPhysicalHelixD gHelix = dcaG.helix();
  gHelix.moveOrigin(gHelix.pathLength(pVtx));
  StThreeVectorF GMomentum = gHelix.momentum(B * kilogauss);
  mGMomentum[0] =  GMomentum.x();
  mGMomentum[1] =  GMomentum.y();
  mGMomentum[2] =  GMomentum.z();
  
  StThreeVectorF Origin = gHelix.origin();
  mOrigin[0] = Origin.x();
  mOrigin[1] = Origin.y();
  mOrigin[2] = Origin.z();

  mDedx      = gTrk->dEdx() * 1.e6;
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
  mNSigmaPion     = TMath::Abs(gTrk->nSigmaPion())     > 1000 ? 1000 : gTrk->nSigmaPion();
  mNSigmaKaon     = TMath::Abs(gTrk->nSigmaKaon())     > 1000 ? 1000 : gTrk->nSigmaKaon();
  mNSigmaProton   = TMath::Abs(gTrk->nSigmaProton())   > 1000 ? 1000 : gTrk->nSigmaProton();
  mNSigmaElectron = TMath::Abs(gTrk->nSigmaElectron()) > 1000 ? 1000 : gTrk->nSigmaElectron();

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
