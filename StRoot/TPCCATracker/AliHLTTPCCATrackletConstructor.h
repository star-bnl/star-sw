/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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

#ifdef V7
  void run( unsigned int firstRow, unsigned int &tracksSaved, unsigned int i_it );
#else
  void run( unsigned int firstRow, unsigned int &tracksSaved );
#endif

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

    // new
  void CreateStartSegmentV( const int rowIndex, const int iter );

  Tracker &fTracker;
  AliHLTArray<TrackletVector> fTrackletVectors;
  SliceData &fData;
};

#endif
