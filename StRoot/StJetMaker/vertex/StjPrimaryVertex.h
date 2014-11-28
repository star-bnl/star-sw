// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

#ifndef STJ_PRIMARY_VERTEX_H
#define STJ_PRIMARY_VERTEX_H

#include "TObject.h"
#include "TVector3.h"

class StjPrimaryVertex : public TObject {
public:
  StjPrimaryVertex()
    : mPosition(-999,-999,-999)
    , mPosError(-999,-999,-999)
    , mVertexFinderId(-999)
    , mRanking(-999)
    , mNTracksUsed(-999)
    , mNBTOFMatch(-999)
    , mNCTBMatch(-999)
    , mNBEMCMatch(-999)
    , mNEEMCMatch(-999)
    , mNCrossCentralMembrane(-999)
    , mSumTrackPt(-999)
    , mMeanDip(-999)
    , mChiSquared(-999)
    , mRefMultPos(-999)
    , mRefMultNeg(-999)
    , mRefMultFtpcEast(-999)
    , mRefMultFtpcWest(-999)
  {
  }

  const TVector3& position()              const { return mPosition; }
  const TVector3& posError()              const { return mPosError; }
  int             vertexFinderId()        const { return mVertexFinderId; }
  float           ranking()               const { return mRanking; }
  short           nTracksUsed()           const { return mNTracksUsed; }
  short           nBTOFMatch()            const { return mNBTOFMatch; }
  short           nCTBMatch()             const { return mNCTBMatch; }
  short           nBEMCMatch()            const { return mNBEMCMatch; }
  short           nEEMCMatch()            const { return mNEEMCMatch; }
  short           nCrossCentralMembrane() const { return mNCrossCentralMembrane; }
  float           sumTrackPt()            const { return mSumTrackPt; }
  float           meanDip()               const { return mMeanDip; }
  float           chiSquared()            const { return mChiSquared; }
  short           refMultPos()            const { return mRefMultPos; }
  short           refMultNeg()            const { return mRefMultNeg; }
  short           refMult()               const { return refMultPos() + refMultNeg(); }
  short           refMultFtpcEast()       const { return mRefMultFtpcEast; }
  short           refMultFtpcWest()       const { return mRefMultFtpcWest; }
  short           refMultFtpc()           const { return refMultFtpcEast() + refMultFtpcWest(); }

private:
  friend class StjTPCMuDst;
  friend class StjMCMuDst;

  TVector3 mPosition;
  TVector3 mPosError;
  int      mVertexFinderId;
  float    mRanking;
  short    mNTracksUsed;
  short    mNBTOFMatch;
  short    mNCTBMatch;
  short    mNBEMCMatch;
  short    mNEEMCMatch;
  short    mNCrossCentralMembrane;
  float    mSumTrackPt;
  float    mMeanDip;
  float    mChiSquared;
  short    mRefMultPos;
  short    mRefMultNeg;
  short    mRefMultFtpcEast;
  short    mRefMultFtpcWest;

  ClassDef(StjPrimaryVertex,0);
};

#endif	// STJ_PRIMARY_VERTEX_H
