// @(#) $Id: AliHLTTPCCAStartHitsFinder.cxx,v 1.1 2016/02/05 23:27:29 fisyak Exp $
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

#include "AliHLTTPCCAStartHitsFinder.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMath.h"

#ifdef USE_TBB
#include <tbb/parallel_sort.h>
#endif //USE_TBB

//////////////////////////////////////////////////////////////////////////////
// depends on:
// - linkUp/linkDown data for the rows above and below the row to process
//
// changes atomically:
// - number of tracklets
//
// writes
// - tracklet start-hit ids
//
// only when all rows are done, is NTracklets known
//////////////////////////////////////////////////////////////////////////////

//X template<typename Int, typename Float>
//X struct SeedRowData {
//X   Int fRow;
//X   Int fHitIndex;
//X };
//X 
//X typedef std::vector<SeedRowData<ushort_v, sfloat_v> > SeedDataVector; // contains ushort_v::Size complete seeds
//X typedef std::vector<SeedRowData<unsigned short, float> > SeedDataScalar; // contains one complete seed
//X 
//X void AliHLTTPCCAStartHitsFinder::run( AliHLTTPCCATracker &tracker, const SliceData &data )
//X {
//X   SeedDataVector seed;
//X   const int lastRow = Parameters::NumberOfRows - 4;
//X   for ( int rowIndex = 2; rowIndex <= lastRow; ++rowIndex ) {
//X     // inside the row we're looking for hits that are linked upwards but not downwards, those start
//X     // a seed
//X     const int maxStorageSize = ( row.NHits() + short_v::Size - 1 ) / short_v::Size;
//X     short_v linkUpCompressedStorage[maxStorageSize];
//X     ushort_v hitIndexesCompressedStorage[maxStorageSize];
//X     short *linkUpCompressed = &linkUpCompressedStorage[0];
//X     unsigned short *hitIndexesCompressed = &hitIndexesCompressedStorage[0];
//X     for ( int hitIndex = 0; hitIndex < row.NHits(); hitIndex += short_v::Size ) {
//X       const short_v linkUp = data.HitLinkUpData( row, hitIndex );
//X       foreach_bit( int i, linkUp != -1 ) {
//X         *linkUpCompressed++ = linkUp[i];
//X         *hitIndexesCompressed++ = hitIndex + i;
//X       }
//X     }
//X   }
//X }

void AliHLTTPCCAStartHitsFinder::run( AliHLTTPCCATracker &tracker, SliceData &data, int iter )
{
  // enum {
  //   kArraySize = 10240,
  //   kMaxStartHits = kArraySize - 1 - short_v::Size
  // };

  AliHLTTPCCAStartHitId *const startHits = tracker.TrackletStartHits();

  //TODO parallel_for
  const int rowStep = AliHLTTPCCAParameters::RowStep;
  const int lastRow = tracker.Param().NRows() - rowStep*2;
  for ( int rowIndex = 0; rowIndex <= lastRow; ++rowIndex ) {
    
#ifdef USE_TBB
    int hitsStartOffset = CAMath::AtomicAdd( tracker.NTracklets(), 0 );
#else //USE_TBB
    const int hitsStartOffset = *tracker.NTracklets(); // number of start hits from other jobs
#endif //USE_TBB
    //X   short_m leftMask( Vc::Zero );
    const AliHLTTPCCARow &row = data.Row( rowIndex );
//    const AliHLTTPCCARow &middleRow = data.Row( rowIndex + rowStep );
    int startHitsCount = 0;


    // look through all the hits and look for
    const int numberOfHits = row.NHits();
    for ( int hitIndex = 0; hitIndex < numberOfHits; hitIndex += short_v::Size ) {
      const short_v hitIndexes = short_v( Vc::IndexesFromZero ) + hitIndex;
      short_m validHitsMask = hitIndexes < numberOfHits;
      validHitsMask &= ( short_v(data.HitDataIsUsed( row ), static_cast<ushort_v>(hitIndexes) ) == short_v( Vc::Zero ) ); // not-used hits can be connected only with not-used, so only one check is needed
      
      // hits that have a link up but none down == the start of a Track
      const short_v &middleHitIndexes = data.HitLinkUpData( row, hitIndex );
      validHitsMask &= ( data.HitLinkDownData( row, hitIndex ) < short_v( Vc::Zero ) ) && ( middleHitIndexes >= short_v( Vc::Zero ) );
      if ( !validHitsMask.isEmpty() ) { // start hit has been found

          // find the length
        int iRow = rowIndex + 1*rowStep;
        int nRows = 2;
        short_v upperHitIndexes = middleHitIndexes;
        for (;!validHitsMask.isEmpty() && nRows < AliHLTTPCCAParameters::NeighboursChainMinLength[iter];) {
          upperHitIndexes = short_v( data.HitLinkUpData( data.Row( iRow ) ), static_cast<ushort_v>( upperHitIndexes ), validHitsMask );
          validHitsMask &= upperHitIndexes >= short_v( Vc::Zero );
          nRows++;
          iRow += rowStep;
        }
          // check if the length is enough
        short_m goodChains = validHitsMask;
        if ( !goodChains.isEmpty() ) { 
            // set all hits in the chain as used
          data.SetHitAsUsed( row, static_cast<ushort_v>( hitIndexes ), goodChains );

          int iRow2 = rowIndex + 1*rowStep;
          ushort_v nHits(Vc::Zero);
          nHits(goodChains) = 2;
          AliHLTTPCCARow curRow2;
          short_v upperHitIndexes2 = middleHitIndexes;
          for (;!goodChains.isEmpty();) {
            curRow2 = data.Row( iRow2 );

            data.SetHitAsUsed( curRow2, static_cast<ushort_v>( upperHitIndexes2 ), goodChains );
            upperHitIndexes2 = short_v( data.HitLinkUpData( curRow2 ), static_cast<ushort_v>( upperHitIndexes2 ), goodChains );
            goodChains &= upperHitIndexes2 >= short_v( Vc::Zero );
            nHits(goodChains)++;
            iRow2 += rowStep;
          }

          foreach_bit( int i, validHitsMask ) {
            startHits[hitsStartOffset + startHitsCount++].Set( rowIndex, hitIndex + i, nHits[i] );
          }

          //   // check free space
          // if ( ISUNLIKELY( startHitsCount >= kMaxStartHits ) ) { // TODO take in account stages kMax for one stage should be smaller
          //   break;
          // }
        } // if good Chains
        
      } // if start hit
    } // for iHit

#ifdef USE_TBB
    hitsStartOffset = CAMath::AtomicAdd( tracker.NTracklets(), startHitsCount ); // number of start hits from other jobs
#else
    *tracker.NTracklets() += startHitsCount;
#endif //USE_TBB
  } // for rowIndex

//   std::cout << *tracker.NTracklets() << " start hits have been found." << std::endl;
#ifdef USE_TBB
  tbb::parallel_sort( startHits, startHits + *tracker.NTracklets() );
#else //USE_TBB
  std::sort( startHits, startHits + *tracker.NTracklets() );
#endif //USE_TBB
}
