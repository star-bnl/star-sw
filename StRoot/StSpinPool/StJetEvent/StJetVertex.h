// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 April 2010
//

// Replica of StMuPrimaryVertex

#ifndef ST_JET_VERTEX_H
#define ST_JET_VERTEX_H

class TLorentzVector;
class StJetCandidate;

#include "TObject.h"
#include "TVector3.h"
#include "TRefArray.h"

class StJetVertex : public TObject {
public:
  StJetVertex()
    : mPosition(-999,-999,-999)
    , mPosError(-999,-999,-999)
    , mVertexFinderId(0)
    , mRanking(-999)
    , mNTracksUsed(0)
    , mNBTOFMatch(0)
    , mNCTBMatch(0)
    , mNBEMCMatch(0)
    , mNEEMCMatch(0)
    , mNCrossCentralMembrane(0)
    , mSumTrackPt(-999)
    , mMeanDip(-999)
    , mChiSquared(9999)
    , mRefMultPos(0)
    , mRefMultNeg(0)
    , mRefMultFtpcWest(0)
    , mRefMultFtpcEast(0)
  {
  }

  const TVector3&  position() const { return mPosition; }
  const TVector3&  posError() const { return mPosError; }
  int        vertexFinderId() const { return mVertexFinderId; }
  float             ranking() const { return mRanking; }
  int           nTracksUsed() const { return mNTracksUsed; }
  int            nBTOFMatch() const { return mNBTOFMatch; }
  int             nCTBMatch() const { return mNCTBMatch; }
  int            nBEMCMatch() const { return mNBEMCMatch; }
  int            nEEMCMatch() const { return mNEEMCMatch; }
  int nCrossCentralMembrane() const { return mNCrossCentralMembrane; }
  float          sumTrackPt() const { return mSumTrackPt; }
  float             meanDip() const { return mMeanDip; }
  float          chiSquared() const { return mChiSquared; }
  int            refMultPos() const { return mRefMultPos; }
  int            refMultNeg() const { return mRefMultNeg; }
  int               refMult() const { return refMultPos() + refMultNeg(); }
  int       refMultFtpcEast() const { return mRefMultFtpcEast; }
  int       refMultFtpcWest() const { return mRefMultFtpcWest; }
  int           refMultFtpc() const { return refMultFtpcWest() + refMultFtpcEast(); }

  const TRefArray& jets() const { return mJets; }
  int numberOfJets() const { return mJets.GetEntriesFast(); }
  StJetCandidate* jet(int i) const { return (StJetCandidate*)mJets.At(i); }
  StJetCandidate* addJet(StJetCandidate* jet) { mJets.Add((TObject*)jet); return (StJetCandidate*)mJets.Last(); }

  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;
  friend class StJetMaker2012;
  friend class StUEMaker2009;

private:
  TVector3 mPosition;
  TVector3 mPosError;
  int mVertexFinderId;
  float mRanking;
  int mNTracksUsed;
  int mNBTOFMatch;
  int mNCTBMatch;
  int mNBEMCMatch;
  int mNEEMCMatch;
  int mNCrossCentralMembrane;
  float mSumTrackPt;
  float mMeanDip;
  float mChiSquared;
  int mRefMultPos;
  int mRefMultNeg;
  int mRefMultFtpcWest;
  int mRefMultFtpcEast;
  TRefArray mJets;

  ClassDef(StJetVertex,1);
};

#endif	// ST_JET_VERTEX_H
