//-*- Mode: C++ -*-

//* This file is property of and copyright by the ALICE HLT Project        *
//* ALICE Experiment at CERN, All rights reserved.                         *
//* See cxx source for full Copyright notice                               *

#ifndef ALIHLTTPCCATRACKLETCONSTRUCTOR_H
#define ALIHLTTPCCATRACKLETCONSTRUCTOR_H

#include "AliHLTTPCCADef.h"
#include <AliHLTArray.h>

class AliHLTTPCCASliceData;

/**
 * @class AliHLTTPCCATrackletConstructor
 *
 */
class AliHLTTPCCATrackletConstructor {
  public:
    inline AliHLTTPCCATrackletConstructor( Tracker &tracker, SliceData &data,
        AliHLTArray<TrackletVector> trackletVectors )
      : fTracker( tracker ), fTrackletVectors( trackletVectors ), fData( data ) {}

    void run();

    struct TrackMemory;
  private:
    void FitTracklet( TrackMemory &r, int rowIndex, ushort_v trackIndex, TrackletVector &trackletVector );
    short_m ExtrapolateTracklet( TrackMemory &r, int rowIndex, ushort_v trackIndex, TrackletVector &trackletVector, const short_m &mask );

    Tracker &fTracker;
    AliHLTArray<TrackletVector> fTrackletVectors;
    SliceData &fData;
};

#endif
