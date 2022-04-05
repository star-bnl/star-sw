// -*- mode: C++ -*-

//
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky
// 8 March 2013
//

#ifndef STJ_ABSTRACT_TRACK_REGION_H
#define STJ_ABSTRACT_TRACK_REGION_H

// ROOT
#include "TObject.h"

// STAR
#include "StjTrackList.h"
#include "StSpinPool/StJetEvent/StJetCandidate.h"

class StjAbstractTrackRegion : public TObject {
public:
  StjAbstractTrackRegion() {}
  virtual ~StjAbstractTrackRegion() {}

  StjTrackList operator()(const StjTrackList& trackList, const StJetCandidate* jet, const TString name)
  {
    return Do(trackList, jet, name);
  }

  virtual StjTrackList Do(const StjTrackList& trackList, const StJetCandidate* jet, const TString name) = 0;

  ClassDef(StjAbstractTrackRegion,0);
};

#endif // STJ_ABSTRACT_TRACK_REGION_H

