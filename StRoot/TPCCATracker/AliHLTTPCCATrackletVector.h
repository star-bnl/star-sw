/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2009 Matthias Kretz <kretz@kde.org>
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

#ifndef ALIHLTTPCCATRACKLETVECTOR_H
#define ALIHLTTPCCATRACKLETVECTOR_H

#include "AliHLTArray.h"
#include "AliHLTTPCCAParameters.h"
#include "AliHLTTPCCATrackParamVector.h"

#include "debug.h"

//#ifndef NVALGRIND
//#include <valgrind/memcheck.h>
//#endif

class AliHLTTPCCATrackletVector
{
  public:
    AliHLTTPCCATrackletVector();

    uint_m IsValid() const { return fNHits > uint_v( Vc::Zero ); }

    uint_v NHits() const    { return fNHits;    }
    uint_v FirstRow() const { return fFirstRow; }
    uint_v LastRow() const  { return fLastRow;  }
    const AliHLTTPCCATrackParamVector &Param() const { return fParam; }

    unsigned int HitIndexAtRow( int rowIndex, int trackIndex ) const {
      return fRowHits[rowIndex][trackIndex];
    }
    uint_v HitIndexAtRow( int rowIndex ) const { return uint_v( &fRowHits[rowIndex][0] ); }

    void SetNHits   ( const uint_v &x ) { fNHits    = x; }
    void SetFirstRow( const uint_v &x ) { fFirstRow = x; }
    void SetLastRow ( const uint_v &x ) { fLastRow  = x; }
    void SetParam   ( const TrackParamVector &x ) { fParam = x; }

    void SetNHits   ( const uint_v &x, uint_m mask ) { fNHits(mask)    = x; }
    void SetFirstRow( const uint_v &x, uint_m mask ) { fFirstRow(mask) = x; }
    void SetLastRow ( const uint_v &x, uint_m mask  ) { fLastRow(mask)  = x; }
    void SetParam   ( const TrackParamVector &x, float_m mask  );

    void SetRowHits( int rowIndex, const uint_v &trackIndex, const uint_v &hitIndex );
    void SetRowHits( const uint_v &rowIndex, const uint_v &trackIndex, const uint_v &hitIndex );
    void SetRowHits( int rowIndex, const uint_v &trackIndex, const uint_v &hitIndex, const int_m &mask );
    void SetRowHits( const uint_v &rowIndex, const uint_v &trackIndex, const uint_v &hitIndex, const int_m &mask );

    void AddHitIds( const uint_v &rowIndexes, const uint_v &hitIndexes, const uint_m &mask );

    int MaxNRows() const { return fRowHits.size(); }
  private:
    uint_v fNHits;      // N hits
    uint_v fFirstRow;   // first TPC row
    uint_v fLastRow;    // last TPC row
    TrackParamVector fParam;   // tracklet parameters
    Vc::array< Vc::array<unsigned int, uint_v::Size>, AliHLTTPCCAParameters::MaxNumberOfRows8 > fRowHits; // hit index for each TPC row
};

inline void AliHLTTPCCATrackletVector::SetParam( const TrackParamVector &x, float_m mask )
{
  if( mask.isFull() ) {
    fParam = x;
    return;
  }
  else {
    fParam.SetTrackParam(x, mask);
  }
}

inline void AliHLTTPCCATrackletVector::SetRowHits( int rowIndex, const uint_v &trackIndex,
    const uint_v &hitIndex )
{
  assert( (trackIndex[0] + uint_v( Vc::IndexesFromZero ) == trackIndex).isFull() );
  assert( uint_m(( trackIndex[0] % uint_v::Size ) == 0).isFull() );
  UNUSED_PARAM1( trackIndex );
  VALGRIND_CHECK_VALUE_IS_DEFINED( hitIndex );
  VALGRIND_CHECK_MEM_IS_DEFINED( &fRowHits[rowIndex], sizeof( uint_v ) );
//  hitIndex.store( &fRowHits[rowIndex][0] );
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    fRowHits[rowIndex][i] = hitIndex[i];
  }
//  VALGRIND_CHECK_MEM_IS_DEFINED( &fRowHits[rowIndex], sizeof( uint_v ) );
}

//inline void AliHLTTPCCATrackletVector::SetRowHits( const uint_v &rowIndexes, const uint_v &trackIndex,
//    const uint_v &hitIndex )
//{
//  assert( (trackIndex[0] + uint_v( Vc::IndexesFromZero ) == trackIndex).isFull() );
//  assert( uint_m(( trackIndex[0] % uint_v::Size ) == 0).isFull() );
//  UNUSED_PARAM1( trackIndex );
//  VALGRIND_CHECK_VALUE_IS_DEFINED( hitIndex );
//  std::cout<<"<!!!ERROR!!!> SetRowHits - Does not work now. Has to be changed or deleted\n";
//}

inline void AliHLTTPCCATrackletVector::SetRowHits( int rowIndex, const uint_v &trackIndex,
    const uint_v &hitIndex, const int_m &mask )
{
  VALGRIND_CHECK_VALUE_IS_DEFINED( rowIndex );
  VALGRIND_CHECK_VALUE_IS_DEFINED( trackIndex );
  VALGRIND_CHECK_VALUE_IS_DEFINED( hitIndex );
  VALGRIND_CHECK_VALUE_IS_DEFINED( mask );
  assert( (trackIndex[0] + uint_v( Vc::IndexesFromZero ) == trackIndex).isFull() );
  assert( uint_m(( trackIndex[0] % uint_v::Size ) == 0).isFull() );
  UNUSED_PARAM1( trackIndex );
  assert( &fRowHits[0] != 0 );
  VALGRIND_CHECK_MEM_IS_DEFINED( &fRowHits[rowIndex], sizeof( uint_v ) );
//   debugF() << "TrackletVector::SetRowHits " << rowIndex << " old: ";
//   debugF() << uint_v( fRowHits[rowIndex] );
//   debugF() << " new: " << hitIndex << mask;
  if( mask.isEmpty() ) return;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    fRowHits[rowIndex][i] = hitIndex[i];
  }
//   debugF() << " done: " << uint_v( fRowHits[rowIndex] ) << std::endl;
//  VALGRIND_CHECK_MEM_IS_DEFINED( &fRowHits[rowIndex], sizeof( uint_v ) );
}

//inline void AliHLTTPCCATrackletVector::SetRowHits( const uint_v &rowIndexes, const uint_v &trackIndex,
//    const uint_v &hitIndex, const int_m &mask )
//{
//  assert( (trackIndex[0] + uint_v( Vc::IndexesFromZero ) == trackIndex).isFull() );
//  assert( uint_m(( trackIndex[0] % uint_v::Size ) == 0).isFull() );
//  UNUSED_PARAM1( trackIndex );
//  VALGRIND_CHECK_VALUE_IS_DEFINED( hitIndex );
//  std::cout<<"<!!!ERROR!!!> SetRowHits - Does not work now. Has to be changed or deleted\n";
//}

typedef AliHLTTPCCATrackletVector TrackletVector;

#endif // ALIHLTTPCCATRACKLETVECTOR_H
