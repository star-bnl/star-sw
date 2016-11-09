#include <limits>

#include "TMath.h"

#include "StPicoEvent/StPicoTrack.h"
#include "St_base/StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"


//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack() : TObject(),
  mId(0),
  mChi2(std::numeric_limits<unsigned short>::max()),
  mPMomentum(0., 0., 0.), mGMomentum(0., 0., 0.), mOrigin(0., 0., 0.),
  mDedx(0.), mDnDx(0.), mDnDxError(0.), mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0), mCharge(0),
  mNSigmaPion(std::numeric_limits<short>::max()),
  mNSigmaKaon(std::numeric_limits<short>::max()),
  mNSigmaProton(std::numeric_limits<short>::max()),
  mNSigmaElectron(std::numeric_limits<short>::max()),
  mTopologyMap{}, mBEmcPidTraitsIndex(-1), mBTofPidTraitsIndex(-1), mMtdPidTraitsIndex(-1)
{
}

//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack(StMuTrack const* const gTrk, StMuTrack const* const pTrk, double const B, StThreeVectorD const& pVtx, StDcaGeometry const& dcaG)
  : StPicoTrack()
{
  if (!gTrk || gTrk->type() != global || (pTrk && (pTrk->type() != primary || pTrk->id() != gTrk->id())))
  {
    LOG_WARN << " Bad StPicoTrack constructor ...... Check!" << endm;
    return;
  }

  mId        = (UShort_t)gTrk->id();
  mChi2      = (gTrk->chi2() * 1000. > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)(TMath::Nint(gTrk->chi2() * 1000.));

  if (pTrk)
  {
    mPMomentum = pTrk->p();
  }

  // Calculate global momentum and position at point of DCA to the pVtx
  StPhysicalHelixD gHelix = dcaG.helix();
  gHelix.moveOrigin(gHelix.pathLength(pVtx));
  mGMomentum = gHelix.momentum(B * kilogauss);
  mOrigin = gHelix.origin();

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
  mNSigmaPion     = (fabs(gTrk->nSigmaPion() * 100.)     > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(gTrk->nSigmaPion() * 100.));
  mNSigmaKaon     = (fabs(gTrk->nSigmaKaon() * 100.)     > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(gTrk->nSigmaKaon() * 100.));
  mNSigmaProton   = (fabs(gTrk->nSigmaProton() * 100.)   > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(gTrk->nSigmaProton() * 100.));
  mNSigmaElectron = (fabs(gTrk->nSigmaElectron() * 100.) > std::numeric_limits<short>::max()) ? std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(gTrk->nSigmaElectron() * 100.));

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
