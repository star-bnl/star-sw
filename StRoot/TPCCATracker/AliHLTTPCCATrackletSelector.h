//-*- Mode: C++ -*-

//* This file is property of and copyright by the ALICE HLT Project        *
//* ALICE Experiment at CERN, All rights reserved.                         *
//* See cxx source for full Copyright notice                               *

#ifndef ALIHLTTPCCATRACKLETSELECTOR_H
#define ALIHLTTPCCATRACKLETSELECTOR_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCATrackletVector.h"
#include "AliHLTArray.h"

/**
 * @class AliHLTTPCCATrackletSelector
 *
 */
class AliHLTTPCCATrackletSelector {
  public:
    inline AliHLTTPCCATrackletSelector( const Tracker &tracker, std::vector<AliHLTTPCCATrack *> *tracks,
        int *numberOfHits, int *numberOfTracks, const SliceData &data,
        AliHLTArray<TrackletVector> &trackletVectors )
      : fTracker( tracker ), fTracks( *tracks ), fNumberOfHits( *numberOfHits ),
      fNumberOfTracks( *numberOfTracks ), fTrackletVectors( trackletVectors ), fData( data )
    {}

    void run();

  private:
    const Tracker &fTracker;
    std::vector<AliHLTTPCCATrack *> &fTracks;
    int &fNumberOfHits;
    int &fNumberOfTracks;
    const AliHLTArray<TrackletVector> fTrackletVectors;
    const SliceData &fData;
};


#endif
