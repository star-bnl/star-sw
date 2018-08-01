/// C+++ headers
#include <limits>

/// ROOT headers
#include <TMath.h>

/// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoTrack.h"

ClassImp(StPicoTrack)

//_________________
StPicoTrack::StPicoTrack() : TObject(),
  mId(0),
  mChi2(std::numeric_limits<unsigned short>::max()),
  mPMomentumX(0), mPMomentumY(0), mPMomentumZ(0),
  mGMomentumX(0), mGMomentumY(0), mGMomentumZ(0),
  mOriginX(0),mOriginY(0), mOriginZ(0),
  mDedx(0), mDedxError(0), /*mDnDx(0),mDnDxError(0)*/
  mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0),
  mNSigmaPion(-30), mNSigmaKaon(-30), mNSigmaProton(-30),
  mNSigmaElectron(-30),
  mTopologyMap{}, mBEmcPidTraitsIndex(-1), mBTofPidTraitsIndex(-1),
  mMtdPidTraitsIndex(-1) {
  /* empty */
}

//_________________
StPicoTrack::StPicoTrack(const StPicoTrack &track) : TObject() {
  mId = track.mId;
  mChi2 = track.mChi2;
  mPMomentumX = track.mPMomentumX;
  mPMomentumY = track.mPMomentumY;
  mPMomentumZ = track.mPMomentumZ;
  mGMomentumX = track.mGMomentumX;
  mGMomentumY = track.mGMomentumY;
  mGMomentumZ = track.mGMomentumZ;
  mOriginX = track.mOriginX;
  mOriginY = track.mOriginY;
  mOriginZ = track.mOriginZ;
  mDedx = track.mDedx;
  mDedxError = track.mDedxError;
  //mDnDx = track.mDnDx;
  //mDnDxError = track.mDnDxError;
  mNHitsFit = track.mNHitsFit;
  mNHitsMax = track.mNHitsMax;
  mNHitsDedx = track.mNHitsDedx;
  mNSigmaPion = track.mNSigmaPion;
  mNSigmaKaon = track.mNSigmaKaon;
  mNSigmaProton = track.mNSigmaProton;
  mNSigmaElectron = track.mNSigmaElectron;
  for(int iIter=0; iIter<2; iIter++) {
    mTopologyMap[iIter] = track.mTopologyMap[iIter];
  }
  mBEmcPidTraitsIndex = track.mBEmcPidTraitsIndex;
  mBTofPidTraitsIndex = track.mBTofPidTraitsIndex;
  mMtdPidTraitsIndex = track.mMtdPidTraitsIndex;
}

//_________________
StPicoTrack::~StPicoTrack() {
  /* emtpy */
}

//_________________
void StPicoTrack::Print(Char_t const* option) const {
  LOG_INFO << "id: " << id()
           << " chi2: " << chi2() << "\n"
           << "pMom: " << pMom().X() << " " << pMom().Y() << " " << pMom().Z()
	   << "gMom: " << gMom().X() << " " << gMom().Y() << " " << gMom().Z()
	   << "origin: " << origin().X() << " " << origin().Y() << " " << origin().Z()
           << " nHitsFit: " << nHitsFit()
           << " nHitsdEdx: " << nHitsDedx() << "\n"
           << "nSigma pi/K/p/e: " << nSigmaPion()   << "/" << nSigmaKaon() << "/"
           << nSigmaProton() << "/" << nSigmaElectron() << "\n"
           << endm;
}

//_________________
Float_t StPicoTrack::gDCAxy(Float_t x, Float_t y) const {
  return TMath::Sqrt( (mOriginX-x)*(mOriginX-x) + (mOriginY-y)*(mOriginY-y) );
}

//_________________
Float_t StPicoTrack::gDCA(Float_t x, Float_t y, Float_t z) const {
  return TMath::Sqrt( (mOriginX-x)*(mOriginX-x) +
		      (mOriginY-y)*(mOriginY-y) +
		      (mOriginZ-z)*(mOriginZ-z) );
}

//_________________
TVector3 StPicoTrack::gDCA(TVector3 pVtx) const {
  return (origin() - pVtx);
}

//_________________
void StPicoTrack::setChi2(Float_t chi2) {
  mChi2 = (chi2 * 1000. > std::numeric_limits<unsigned short>::max()) ?
    std::numeric_limits<unsigned short>::max() : (UShort_t)(TMath::Nint(chi2 * 1000.));
}

//_________________
void StPicoTrack::setDedx(Float_t dEdx) {
  /// In KeV/cm
  mDedx = dEdx * 1.e6;
}

//_________________
void StPicoTrack::setDedxError(Float_t dEdxError) {
  /// In KeV/cm
  mDedxError = dEdxError * 1.e6;
}

//_________________
void StPicoTrack::setNHitsMax(Int_t nhits) {
  mNHitsMax = (UChar_t)nhits;
}

//_________________
void StPicoTrack::setNHitsPossible(Int_t nhits) {
  /// For those who wants to have standard terminology
  setNHitsMax(nhits);
}

//_________________
void StPicoTrack::setNHitsDedx(Int_t nhits) {
  mNHitsDedx = (UChar_t)nhits;
}

//_________________
void StPicoTrack::setTopologyMap(Int_t id, UInt_t word) {
  if(id==0 || id==1) {
    mTopologyMap[id] = word;
  }
  else {
    /// Shouldn't here be a protection?
  }
}

//_________________
TVector3 StPicoTrack::gMom(TVector3 pVtx, Float_t const B) const {
  StPicoPhysicalHelix gHelix = helix(B);
  return gHelix.momentumAt( gHelix.pathLength( pVtx ), B * kilogauss );
}

//_________________
StPicoPhysicalHelix StPicoTrack::helix(Float_t const B) const {
  return StPicoPhysicalHelix( gMom(), origin(), B * kilogauss,
			      static_cast<float>( charge() ) );
}
