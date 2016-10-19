// @(#) $Id: AliHLTTPCCATrackletSelector.cxx,v 1.6 2012/08/14 16:30:42 fisyak Exp $
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
  fTracks.resize( fTrackletVectors.Size() * uint_v::Size * AliHLTTPCCAParameters::MaxNumberOfRows8 / AliHLTTPCCAParameters::MinimumHitsForTrack ); // should be less, the factor is for safety, since the tracks can be broken into pieces
#ifdef USE_TBB
  tbb::atomic<int> numberOfTracks;
  tbb::fatomic<int> NHitsTotal;
#else //USE_TBB
  int numberOfTracks;
  int NHitsTotal;
#endif //USE_TBB

  numberOfTracks = 0;
  NHitsTotal = 0;

  const int NTracklets = fTracker.NTracklets();
  for ( int iTrackletV = 0; iTrackletV * int_v::Size < NTracklets; ++iTrackletV ) {
    const TrackletVector &tracklet = fTrackletVectors[iTrackletV];
    const uint_v trackIndexes = uint_v( Vc::IndexesFromZero ) + uint_v(iTrackletV * int_v::Size);

    const uint_v &NTrackletHits = tracklet.NHits();
    const uint_m &validTracklets = trackIndexes < NTracklets && NTrackletHits >= uint_v(AliHLTTPCCAParameters::MinimumHitsForTracklet);

    const float_v kMaximumSharedPerHits = 1.f / AliHLTTPCCAParameters::MinimumHitsPerShared;

    const uint_v &firstRow = tracklet.FirstRow();
    const uint_v &lastRow  = tracklet.LastRow();

    uint_v nTrackHits( Vc::Zero );

    const uint_v &weight = SliceData::CalculateHitWeight( NTrackletHits, trackIndexes );

    Track *trackCandidates[int_v::Size];
    for(int iV=0; iV<uint_v::Size; iV++)
    {
      if(!validTracklets[iV]) continue;
//    foreach_bit ( int iV, validTracklets ) {
      trackCandidates[iV] = new Track;
    }

    uint_v gap( Vc::Zero ); // count how many rows are missing a hit
    uint_v nShared( Vc::Zero );
    for ( unsigned int rowIndex = firstRow.min(); rowIndex <= lastRow.max(); ++rowIndex ) {
      ++gap;
      const uint_v &hitIndexes = tracklet.HitIndexAtRow( rowIndex ); // hit index for the current row
      const uint_m &validHits = validTracklets && validHitIndexes( hitIndexes );
      const uint_m &ownHitsMask = fData.TakeOwnHits( fData.Row( rowIndex ), hitIndexes, validHits, weight );
      const uint_m &canShareHitMask = nShared < static_cast<uint_v>( static_cast<float_v>( nTrackHits ) * kMaximumSharedPerHits );
      const uint_m &saveHitMask = validHits && ( ownHitsMask || canShareHitMask );
      const uint_m &bigGapMask = gap > static_cast<unsigned int>(AliHLTTPCCAParameters::MaximumRowGap);
      const uint_m &brokenTrackMask = bigGapMask && (nTrackHits >= static_cast<unsigned int>(AliHLTTPCCAParameters::MinimumHitsForTrack));
      for(int iV=0; iV<uint_v::Size; iV++)
      {
        if(!validTracklets[iV]) continue;
//       foreach_bit ( int iV, validTracklets ) {
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

    for(int iV=0; iV<uint_v::Size; iV++)
    {
      if(!validTracklets[iV]) continue;
//     foreach_bit ( int iV, validTracklets ) {
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
