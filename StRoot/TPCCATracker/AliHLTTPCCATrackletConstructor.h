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
    // add one hit from chain to track
  void FitTracklet( TrackMemory &r, const int rowIndex, const uint_v trackIndex, TrackletVector &trackletVector );
    // find nearest hit on row and set it as currentHit (see TrackMemory)
  void FindNextHit( TrackMemory &r, const AliHLTTPCCARow &row,
                    float_v &dy_best, float_v &dz_best, int_m &active);
    // extrapolate on row and try to find hit belonged to track
  int_m ExtrapolateTracklet( TrackMemory &r, const int rowIndex, const uint_v trackIndex, TrackletVector &trackletVector, const bool dir, const int_m &mask );

    // performs both: fitTraclet & Extrapolation
  int_m ExtendTracklet( TrackMemory &r, const int rowIndex, const uint_v trackIndex, TrackletVector &trackletVector, const bool dir, const int_m &mask ); 

  Tracker &fTracker;
  AliHLTArray<TrackletVector> fTrackletVectors;
  SliceData &fData;
};

#endif
