#ifdef _VANILLA_ROOT_
#include <iostream>
#define LOG_INFO std::cout
#define endm std::endl
#else
#include "St_base/StMessMgr.h"
#endif

/// PicoDst headers
#include "StPicoTrackCovMatrix.h"

ClassImp(StPicoTrackCovMatrix)

//_________________
StPicoTrackCovMatrix::StPicoTrackCovMatrix() : TObject(),
  mImp(0), mZ(0), mPsi(0), mPti(0), mTan(0), mCurv(0),
  mSigma{}, mCorr{} {
    /* empty */
}

//_________________
StPicoTrackCovMatrix::StPicoTrackCovMatrix(const StPicoTrackCovMatrix &mtx) {
  mImp = mtx.mImp;
  mZ = mtx.mZ;
  mPsi = mtx.mPsi;
  mPti = mtx.mPti;
  mTan = mtx.mTan;
  mCurv = mtx.mCurv;
  for(Int_t iIter=0; iIter<5; iIter++) {
    mSigma[iIter] = mtx.mSigma[iIter];
  }
  for(Int_t iIter=0; iIter<10; iIter++) {
    mCorr[iIter] = mtx.mCorr[iIter];
  }
}

//_________________
StPicoTrackCovMatrix::~StPicoTrackCovMatrix() {
  /* empty */
}

//_________________
void StPicoTrackCovMatrix::Print(Char_t const* option) const {
  const Float_t *lSigma = sigmas();
  const Float_t *lCorr = correlations();
  LOG_INFO << "imp: " << imp()
	   << "z: " << z()
	   << "psi: " << psi()
	   << "pti: " << pti()
	   << "tan: " << tan()
	   << "curv: " << curv()
	   << "sigmas: \n"
	   << lSigma[0] << "/" << lSigma[1] << "/"
	   << lSigma[2] << "/" << lSigma[3] << "/"
	   << lSigma[4] << "\n"
	   << "correlations: \n"
	   << lCorr[0] << "/" << lCorr[1] << "/"
	   << lCorr[2] << "/" << lCorr[3] << "/"
	   << lCorr[4] << "/" << lCorr[5] << "/"
	   << lCorr[6] << "/" << lCorr[7] << "/"
	   << lCorr[8] << "/" << lCorr[9]    
	   << endm;
}
