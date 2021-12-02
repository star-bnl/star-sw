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


#include "AliHLTTPCCATrackletSelector.h"
#include "AliHLTTPCCATrack.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCATrackParamVector.h"
#include "AliHLTTPCCATracklet.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCAParameters.h"
#include <stack>
#undef USE_TBB
#ifdef USE_TBB
#include <tbb/atomic.h>
#endif //USE_TBB

#include "debug.h"

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#endif // DRAW

using std::endl;

void AliHLTTPCCATrackletSelector::run()
{
  fTracks.resize( fNumberOfTracks * uint_v::Size * AliHLTTPCCAParameters::MaxNumberOfRows8 / AliHLTTPCCAParameters::MinimumHitsForTrack ); // should be less, the factor is for safety, since the tracks can be broken into pieces
#ifdef USE_TBB
  tbb::atomic<int> numberOfTracks;
  tbb::fatomic<int> NHitsTotal;
#else //USE_TBB
  int numberOfTracks;
  int NHitsTotal;
#endif //USE_TBB

  numberOfTracks = 0;
  NHitsTotal = 0;

  const unsigned int NTracklets = fTracker.NTracklets();
  for ( unsigned int iTrackletV = 0; iTrackletV * int_v::Size < NTracklets; ++iTrackletV ) {
    const TrackletVector &tracklet = fTrackletVectors[iTrackletV];
    const uint_v trackIndexes = uint_v( Vc::IndexesFromZero ) + uint_v(iTrackletV * int_v::Size);

    const uint_v &NTrackletHits = tracklet.NHits();
    const uint_m &validTracklets = trackIndexes < NTracklets+4 && NTrackletHits >= uint_v(AliHLTTPCCAParameters::MinimumHitsForTracklet);

    const float_v kMaximumSharedPerHits = 1.f / AliHLTTPCCAParameters::MinimumHitsPerShared;

    const uint_v &firstRow = tracklet.FirstRow();
    const uint_v &lastRow  = tracklet.LastRow();

    uint_v nTrackHits( Vc::Zero );

    Track *trackCandidates[int_v::Size];
    for( unsigned int iV=0; iV<uint_v::Size; iV++ ) {
      if(!validTracklets[iV]) continue;
      trackCandidates[iV] = new Track;
    }

    uint_v gap( Vc::Zero ); // count how many rows are missing a hit
    uint_v nShared( Vc::Zero );
    for ( unsigned int rowIndex = firstRow.min(); rowIndex <= lastRow.max(); ++rowIndex ) {
      ++gap;
      const uint_v &hitIndexes = tracklet.HitIndexAtRow( rowIndex ); // hit index for the current row
      const uint_m &validHits = validTracklets && validHitIndexes( hitIndexes );
      const uint_m &ownHitsMask = fData.TakeOwnHits( fData.Row( rowIndex ), hitIndexes, validHits, NTrackletHits );
      const uint_m &canShareHitMask = nShared < static_cast<uint_v>( static_cast<float_v>( nTrackHits ) * kMaximumSharedPerHits );
      const uint_m &saveHitMask = validHits && ( ownHitsMask || canShareHitMask );
      const uint_m &bigGapMask = gap > static_cast<unsigned int>(AliHLTTPCCAParameters::MaximumRowGap);
      const uint_m &brokenTrackMask = bigGapMask && (nTrackHits >= static_cast<unsigned int>(AliHLTTPCCAParameters::MinimumHitsForTrack));
      for( unsigned int iV=0; iV<uint_v::Size; iV++ ) {
        if(!validTracklets[iV]) continue;
        if ( saveHitMask[iV] ) {
          assert( hitIndexes[iV] < fData.Row( rowIndex ).NHits() );
          trackCandidates[iV]->fHitIdArray[nTrackHits[iV]].Set( rowIndex, hitIndexes[iV] );
        } 
        else if ( brokenTrackMask[iV] ) { // save part of the track and create new track from the rest
          NHitsTotal += nTrackHits[iV];

          fTracks[numberOfTracks] = trackCandidates[iV];
          fTracks[numberOfTracks]->fNumberOfHits = nTrackHits[iV];
          fTracks[numberOfTracks]->fParam = TrackParam( tracklet.Param(), iV );
          numberOfTracks++;
        
          trackCandidates[iV] = new Track;
        } // if save
      } // for i
      nTrackHits( saveHitMask )++;
      nTrackHits.setZero( bigGapMask && !saveHitMask );
      nShared( saveHitMask && !ownHitsMask  )++;
      gap.setZero( saveHitMask || brokenTrackMask );
    }

    for( unsigned int iV=0; iV<uint_v::Size; iV++ ) {
      if(!validTracklets[iV]) continue;
      if ( nTrackHits[iV] >= static_cast<unsigned int>(AliHLTTPCCAParameters::MinimumHitsForTrack) ) {
        NHitsTotal += nTrackHits[iV];
        
        fTracks[numberOfTracks] = trackCandidates[iV];
        fTracks[numberOfTracks]->fNumberOfHits = nTrackHits[iV];
        fTracks[numberOfTracks]->fParam = TrackParam( tracklet.Param(), iV );
        numberOfTracks++;
      } else {
        delete trackCandidates[iV];
      }
    }

  } // for iTrackletV
  fNumberOfHits = NHitsTotal;
  fTracks.resize( numberOfTracks );
  fNumberOfTracks = numberOfTracks;
}
