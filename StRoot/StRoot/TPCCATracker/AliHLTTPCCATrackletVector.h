/**************************************************************************
 * This file is property of and copyright by the ALICE HLT Project        *
 * All rights reserved.                                                   *
 *                                                                        *
 * Primary Authors:                                                       *
 *     Copyright 2009       Matthias Kretz <kretz@kde.org>                *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

#ifndef ALIHLTTPCCATRACKLETVECTOR_H
#define ALIHLTTPCCATRACKLETVECTOR_H

#include "AliHLTArray.h"
#include "AliHLTTPCCAParameters.h"
#include "AliHLTTPCCATrackParamVector.h"

#include "debug.h"

class AliHLTTPCCATrackletVector
{
  public:
    AliHLTTPCCATrackletVector();

    ushort_m IsValid() const { return fNHits > ushort_v( Vc::Zero ); }

    ushort_v NHits() const    { return fNHits;    }
    ushort_v FirstRow() const { return fFirstRow; }
    ushort_v LastRow() const  { return fLastRow;  }
    const AliHLTTPCCATrackParamVector &Param() const { return fParam; }

    unsigned short HitIndexAtRow( int rowIndex, int trackIndex ) const {
      return fRowHits[rowIndex][trackIndex];
    }
    ushort_v HitIndexAtRow( int rowIndex ) const { return ushort_v( fRowHits[rowIndex] ); }
    /*
    ushort_v HitIndexAtRows( const ushort_v &rowIndex ) const {
      return ushort_v( &fRowHits[0], rowIndex * ushort_v::Size + ushort_v( Vc::IndexesFromZero ) );
    }
    ushort_v HitIndexAtRows( const ushort_v &rowIndex, const ushort_m &mask ) const {
      return ushort_v( &fRowHits[0], rowIndex * ushort_v::Size + ushort_v( Vc::IndexesFromZero ), mask );
    }
    */

    void SetNHits   ( const ushort_v &x ) { fNHits    = x; }
    void SetFirstRow( const ushort_v &x ) { fFirstRow = x; }
    void SetLastRow ( const ushort_v &x ) { fLastRow  = x; }
    void SetParam   ( const TrackParamVector &x ) { fParam = x; }

    void SetRowHits( int rowIndex, const ushort_v &trackIndex, const ushort_v &hitIndex );
    void SetRowHits( const ushort_v &rowIndex, const ushort_v &trackIndex, const ushort_v &hitIndex );
    void SetRowHits( int rowIndex, const ushort_v &trackIndex, const ushort_v &hitIndex, const short_m &mask );
    void SetRowHits( const ushort_v &rowIndex, const ushort_v &trackIndex, const ushort_v &hitIndex, const short_m &mask );

    void AddHitIds( const ushort_v &rowIndexes, const ushort_v &hitIndexes, const ushort_m &mask );

    int MaxNRows() const { return fRowHits.Size(); }
  private:
    ushort_v fNHits;      // N hits
    ushort_v fFirstRow;   // first TPC row
    ushort_v fLastRow;    // last TPC row
    TrackParamVector fParam;   // tracklet parameters
    AliHLTFixedArray<ushort_v::Memory, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8> > fRowHits; // hit index for each TPC row
};

inline void AliHLTTPCCATrackletVector::SetRowHits( int rowIndex, const ushort_v &trackIndex,
    const ushort_v &hitIndex )
{
  assert( trackIndex[0] + ushort_v( Vc::IndexesFromZero ) == trackIndex );
  assert( ( trackIndex[0] % ushort_v::Size ) == 0 );
  UNUSED_PARAM1( trackIndex );
  hitIndex.store( fRowHits[rowIndex] );
}

inline void AliHLTTPCCATrackletVector::SetRowHits( const ushort_v &rowIndexes, const ushort_v &trackIndex,
    const ushort_v &hitIndex )
{
  assert( trackIndex[0] + ushort_v( Vc::IndexesFromZero ) == trackIndex );
  assert( ( trackIndex[0] % ushort_v::Size ) == 0 );
  UNUSED_PARAM1( trackIndex );
  hitIndex.scatter( fRowHits[0], rowIndexes * ushort_v::Size + ushort_v( Vc::IndexesFromZero ) );
}

inline void AliHLTTPCCATrackletVector::SetRowHits( int rowIndex, const ushort_v &trackIndex,
    const ushort_v &hitIndex, const short_m &mask )
{
  assert( trackIndex[0] + ushort_v( Vc::IndexesFromZero ) == trackIndex );
  assert( ( trackIndex[0] % ushort_v::Size ) == 0 );
  UNUSED_PARAM1( trackIndex );
  assert( &fRowHits[0] != 0 );
  debugF() << "TrackletVector::SetRowHits " << rowIndex << " old: ";
  debugF() << ushort_v( fRowHits[rowIndex] );
  debugF() << " new: " << hitIndex << mask;
  hitIndex.store( fRowHits[rowIndex], mask );
  debugF() << " done: " << ushort_v( fRowHits[rowIndex] ) << std::endl;
}

inline void AliHLTTPCCATrackletVector::SetRowHits( const ushort_v &rowIndexes, const ushort_v &trackIndex,
    const ushort_v &hitIndex, const short_m &mask )
{
  assert( trackIndex[0] + ushort_v( Vc::IndexesFromZero ) == trackIndex );
  assert( ( trackIndex[0] % ushort_v::Size ) == 0 );
  UNUSED_PARAM1( trackIndex );
  hitIndex.scatter( fRowHits[0], rowIndexes * ushort_v::Size + ushort_v( Vc::IndexesFromZero ), mask );
}

typedef AliHLTTPCCATrackletVector TrackletVector;

#endif // ALIHLTTPCCATRACKLETVECTOR_H
