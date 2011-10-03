//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// header file for class StTpcEvalHistograms
//-------------------------------------------------
#ifndef StTpcEvalHistograms_H
#define StTpcEvalHistograms_H

#include "StObject.h"

class TH1F;
class TH2F;
class TNtuple;
class TClonesArray;
class MatchedTrackPair; //!

class StTpcEvalHistograms : public StObject {
 public:
  StTpcEvalHistograms();
  virtual ~StTpcEvalHistograms();
  void Book();
  void fillTrackNtuple(MatchedTrackPair*); //!

  TH1F*     tpcHitResX; //! local x difference
  TH1F*     tpcHitResY; //! local y difference (should be almost identical)
  TH1F*     tpcHitResZ; //! local z difference

  TH1F*     matchesToRcHits; //! 
  TH1F*     matchesToMcHits; //!

  TH1F*     mcHitPositionRad; //! for all M.C. hits
  TH1F*     mcHitPositionZ; //! for all M.C. hits

  TH1F*     mcUnmatchedHitPositionRad; //! for M.C. hits that are not matched to any rec. hit
  TH1F*     mcUnmatchedHitPositionSector; //! for M.C. hits that are not matched to any rec. hit
  TH1F*     mcUnmatchedHitPositionZ; //! for M.C. hits that are not matched to any rec. hit

  TH1F*     mc1to1HitPositionRad; //! for 1 to 1 matched btw Rec & M.C.
  TH1F*     mc1to1HitPositionZ; //! for 1 to 1 matched btw Rec & M.C.
    
  TH1F*     mcMergedHitPositionRad; //! for rec. hits that are matched to more than 1 M.C. hit
  TH1F*     mcMergedHitPositionZ; //! for rec. hits that are matched to more than 1 M.C. hit

  TH1F*     rcHitPositionRad; //! for All rec. hits
  TH1F*     rcHitPositionZ; //! for All rec. hits

  TH1F*     rcUnmatchedHitPositionRad; //! for rec. hits that are not matched to any M.C. hit
  TH1F*     rcUnmatchedHitPositionSector; //! for rec. hits that are not matched to any M.C. hit
  TH1F*     rcUnmatchedHitPositionZ; //! for rec. hits that are not matched to any M.C. hit

  TNtuple*  trackNtuple; //!
    Int_t         mTrackIndex;
    Int_t         mMcMultiplicity;
    Int_t         mRcMultiplicity;
    Int_t         mPairMultiplicity;
 private:
  ClassDef(StTpcEvalHistograms,1)
};

#endif
