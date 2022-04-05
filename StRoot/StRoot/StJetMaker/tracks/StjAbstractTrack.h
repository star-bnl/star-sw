// -*- mode: C++ -*-

//
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky
// 8 March 2013
//

#ifndef STJ_ABSTRACT_TRACK_H
#define STJ_ABSTRACT_TRACK_H

// ROOT
#include "TObject.h"

// STAR
#include "StjTrackList.h"


class StjAbstractTrack : public TObject {
public:
  StjAbstractTrack() {}
  virtual ~StjAbstractTrack() {}

  StjTrackList operator()(const StjTrackList& trackList)
  {
    return Do(trackList);
  }

  virtual StjTrackList Do(const StjTrackList& trackList) = 0;

  ClassDef(StjAbstractTrack,0);
};

#endif // STJ_ABSTRACT_TRACK_H

