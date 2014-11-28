// -*- mode: C++ -*-

//
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky
// 6 March 2012
//

#ifndef STJ_TRACK_PT_FRACTION_H
#define STJ_TRACK_PT_FRACTION_H

// STAR
#include "StjTrackList.h"

//Local
#include "StjAbstractTrack.h"

class StjTrackPtFraction : public StjAbstractTrack {
public:
  StjTrackPtFraction(float fraction) : mFraction(fraction) {}
  virtual ~StjTrackPtFraction() {}
  
  StjTrackList Do(const StjTrackList& trackList);
  
  float fraction() const { return mFraction; }
  void setFraction(float fraction) { mFraction = fraction; }

private:
  float mFraction;

  ClassDef(StjTrackPtFraction,0);

};

#endif // STJ_TRACK_PT_FRACTION_H
