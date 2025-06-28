#include "StPicoFwdTrack.h"
#include <vector>
#include "StPicoMessMgr.h"

// ROOT headers
#include "TMath.h"

ClassImp(StPicoFwdTrack)

StPicoFwdTrack::StPicoFwdTrack() : TObject() {
    /* No Op*/
}

StPicoFwdTrack::StPicoFwdTrack(const StPicoFwdTrack &fwdTrack){
    mId = fwdTrack.mId;
    mNumberOfSeedPoints = fwdTrack.mNumberOfSeedPoints;
    mNumberOfFitPoints = fwdTrack.mNumberOfFitPoints;
    mChi2 = fwdTrack.mChi2;
    mPVal = fwdTrack.mPVal;

    mMomentumX = fwdTrack.mMomentumX;
    mMomentumY = fwdTrack.mMomentumY;
    mMomentumZ = fwdTrack.mMomentumZ;
    mStatus = fwdTrack.mStatus;
    
    mIdTruth = fwdTrack.mIdTruth;
    mQATruth = fwdTrack.mQATruth;

    for ( size_t i = 0 ; i < fwdTrack.mEcalMatchIndex.size(); i++ ){
      addEcalCluster( fwdTrack.mEcalMatchIndex[i] );
    }
    for ( size_t i = 0 ; i < fwdTrack.mHcalMatchIndex.size(); i++ ){
      addHcalCluster( fwdTrack.mHcalMatchIndex[i] );
    }

    mDCAXY = fwdTrack.mDCAXY;
    mDCAZ = fwdTrack.mDCAZ;
    mVtxIndex = fwdTrack.mVtxIndex;
    mGlobalTrackIndex = fwdTrack.mGlobalTrackIndex;

    mECalX = fwdTrack.mECalX;
    mECalY = fwdTrack.mECalY;
    mECalZ = fwdTrack.mECalZ;
    mHCalX = fwdTrack.mHCalX;
    mHCalY = fwdTrack.mHCalY;
    mHCalZ = fwdTrack.mHCalZ;
}

StPicoFwdTrack::~StPicoFwdTrack(){

}

//_________________
void StPicoFwdTrack::Print(const Char_t* option __attribute__((unused))) const {
  LOG_INFO << " chi2: " << chi2() << "\n"
           << "pVal: " << pVal() << "\n"
           << "pMom: " << momentum().X() << " " << momentum().Y() << " " << momentum().Z() << "\n"
           << "nHitsFit: " << numberOfFitPoints()
           << " numberOfSeedPoints: " << numberOfSeedPoints() << "\n"
           << "idTruth: " << idTruth() << " qaTruth: " << qaTruth() << "\n"
           << endm;
}

//_________________
void StPicoFwdTrack::setChi2(Float_t chi2) {
  mChi2 = ( (chi2 * 1000.) > std::numeric_limits<unsigned short>::max() ?
	    std::numeric_limits<unsigned short>::max() :
	    (UShort_t)( TMath::Nint( chi2 * 1000. ) ) );
}
void StPicoFwdTrack::setPVal(Float_t pval) {
  mPVal = ( (pval * 10000.f) > std::numeric_limits<unsigned short>::max() ?
	    std::numeric_limits<unsigned short>::max() :
	    (UShort_t)( TMath::Nint( pval * 10000.f ) ) );
}