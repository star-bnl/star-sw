// @(#) $Id: AliHLTTPCCATrackletSelector.cxx,v 1.2 2016/07/15 14:43:33 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************


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
  fTracks.resize( fTrackletVectors.Size() * ushort_v::Size * AliHLTTPCCAParameters::MaxNumberOfRows8 / AliHLTTPCCAParameters::MinimumHitsForTrack ); // should be less, the factor is for safety, since the tracks can be broken into pieces
#ifdef USE_TBB
  tbb::atomic<int> numberOfTracks;
  tbb::atomic<int> numberOfHits;
#else //USE_TBB
  int numberOfTracks;
  int numberOfHits;
#endif //USE_TBB

  numberOfTracks = -1;
  std::stack<AliHLTTPCCATrack *> recycleBin;

  numberOfHits = 0;

  const int fNTracklets = fTracker.NTracklets();
  debugTS() << "run TrackletSelector on " << fNTracklets << " tracklets" << endl;
  for ( int itr = 0; itr * short_v::Size < fNTracklets; ++itr ) {
    const ushort_v trackIndexes = ushort_v( Vc::IndexesFromZero ) + itr * short_v::Size;
    // Tracklets that will be examined
    const TrackletVector &tracklet = fTrackletVectors[itr];
    debugTS() << "process " << trackIndexes << "\n" << tracklet.Param() << endl;;

    // the Tracklet says it contains so many hits
    const ushort_v &tNHits = tracklet.NHits();

    // useless Tracklet...
    const ushort_m &valid = trackIndexes < fNTracklets && tNHits > 0;

    const sfloat_v kMaximumSharedPerHits = 1.f / AliHLTTPCCAParameters::MinimumHitsPerShared;

    const ushort_v &firstRow = tracklet.FirstRow();
    const ushort_v &lastRow  = tracklet.LastRow();

    ushort_v tNHitsNew( Vc::Zero );

    const ushort_v &weight = SliceData::CalculateHitWeight( tNHits, trackIndexes );

    debugTS() << "tNHits: " << tNHits << ", valid: " << valid << ", firstRow: " << firstRow << ", lastRow: " << lastRow << ", weight: " << weight << endl;

    Track *tracks[short_v::Size];
    {
      int i = 0;
      // XXX lock
      for ( ; !recycleBin.empty() && i < short_v::Size; ++i ) {
        tracks[i] = recycleBin.top();
        recycleBin.pop();
        tracks[i]->fParam = TrackParam( tracklet.Param(), i );
      }
      // XXX unlock
      for ( ; i < short_v::Size; ++i ) {
        tracks[i] = new Track;
        fTracks[++numberOfTracks] = tracks[i];
        tracks[i]->fParam = TrackParam( tracklet.Param(), i );
      }
    }

    //const int nRows = fTracker.Param().NRows(); // number of rows (Tracker global)
    ushort_v gap( Vc::Zero ); // count how many rows are missing a hit
    ushort_v nShared( Vc::Zero );
    const ushort_v &invalidMarker = std::numeric_limits<ushort_v>::max();
    for ( int rowIndex = firstRow.min(); rowIndex <= lastRow.max(); ++rowIndex ) {
      ++gap;
      const ushort_v &hitIndexes = tracklet.HitIndexAtRow( rowIndex ); // hit index for the current row
      debugTS() << hitIndexes << invalidMarker << validHitIndexes( hitIndexes ) << endl;
      const ushort_m &validHits = valid && validHitIndexes( hitIndexes );
      //cerr << rowIndex << hitIndexes << weight << fData.HitWeight( fData.Row( rowIndex ), hitIndexes, validHits ) << endl;
      const ushort_m own = fData.TakeOwnHits( fData.Row( rowIndex ), hitIndexes, validHits, weight );
      //cerr << own << fData.HitWeight( fData.Row( rowIndex ), hitIndexes, validHits ) << endl;
      //const ushort_m &own = fData.HitWeight( fData.Row( rowIndex ), hitIndexes, validHits ) == weight;
      const ushort_m &sharedOK = nShared < static_cast<ushort_v>( static_cast<sfloat_v>( tNHitsNew ) * kMaximumSharedPerHits );
      const ushort_m &outHit = validHits && ( own || sharedOK );
#ifndef NODEBUG
      const ushort_m &invalidTrack = !( own || sharedOK );
//      std::cout << invalidTrack << "       "<< outHit <<"            "<<firstRow.min()<<"  "<<lastRow.max()<<"         "<< firstRow<<"  "<<lastRow<<std::endl;
      if ( !invalidTrack.isEmpty() ) {
        debugTS() << "invalidTrack at row " << rowIndex << ": " << invalidTrack
          << ", own: " << own
          << ", sharedOK: " << sharedOK
          << endl;
        debugTS() << "weight reference: " << fData.HitWeight( fData.Row( rowIndex ), hitIndexes, validHits ) << validHits << endl;
      }
#endif
      for ( int i = 0; i < short_v::Size; ++i ) {
        if ( outHit[i] ) {
          assert( hitIndexes[i] < fData.Row( rowIndex ).NHits() );
          tracks[i]->fHitIdArray[tracks[i]->fNumberOfHits++].Set( rowIndex, hitIndexes[i] );
        } else if ( static_cast<int>(gap[i]) > static_cast<int>(AliHLTTPCCAParameters::MaximumRowGap) ) {
          if ( tracks[i]->fNumberOfHits >= AliHLTTPCCAParameters::MinimumHitsForTrack ) {
            numberOfHits += tracks[i]->fNumberOfHits;
            tracks[i] = new Track;
            fTracks[++numberOfTracks] = tracks[i];
          } else {
            tracks[i]->fNumberOfHits = 0;
          }
        }
      }
      gap.setZero( outHit );
      ++tNHitsNew( outHit );
      tNHitsNew.setZero( gap > ushort_v(AliHLTTPCCAParameters::MaximumRowGap) );
      ++nShared( !own && outHit );
    }

    for ( int i = 0; i < short_v::Size; ++i ) {
      if ( tracks[i]->fNumberOfHits < ushort_v(AliHLTTPCCAParameters::MinimumHitsForTrack) ) {
        tracks[i]->fNumberOfHits = 0;
        // XXX lock
        recycleBin.push( tracks[i] );
        // XXX unlock
      } else {
        numberOfHits += tracks[i]->fNumberOfHits;
      }
    }

    //debugTS() << "NTracks: " << fTracker.NTracks() << ", NTrackHits: " << fTracker.NTrackHits() << endl;
  }
  fNumberOfHits = numberOfHits;
  fTracks.resize( numberOfTracks + 1, 0 );
  fNumberOfTracks = numberOfTracks + 1 - recycleBin.size();
}
