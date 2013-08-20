/*
    Copyright (C) 2009 Matthias Kretz <kretz@kde.org>

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) version 3.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.

*/

#include "AliHLTTPCCASliceDataVector.h"
#include "AliHLTTPCCAClusterData.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAHit.h"
#include "AliHLTTPCCAParam.h"
#include "MemoryAssignmentHelpers.h"
#include <iostream>
#ifndef NVALGRIND 
#include <valgrind/memcheck.h>
#endif

// calculates an approximation for 1/sqrt(x)
// Google for 0x5f3759df :)
static inline float fastInvSqrt( float _x )
{
  union { float f; int i; } x = { _x };
  const float xhalf = 0.5f * x.f;
  x.i = 0x5f3759df - ( x.i >> 1 );
  x.f = x.f * ( 1.5f - xhalf * x.f * x.f );
  return x.f;
}

inline void AliHLTTPCCASliceData::createGrid( AliHLTTPCCARow *row, const AliHLTTPCCAClusterData &data, const int clusterDataOffset )
{
  if ( row->NHits() <= 0 ) { // no hits or invalid data
    // grid coordinates don't matter, since there are no hits
    row->fGrid.CreateEmpty();
    return;
  } else if ( row->NHits() == 1 ) {
    const float y = data.Y( clusterDataOffset );
    const float z = data.Z( clusterDataOffset );
    row->fGrid.Create1( y, z, 2.f, 2.f );
    return;
  }

  const float norm = fastInvSqrt( row->fNHits );

  float yMin =  1.e3f;
  float yMax = -1.e3f;
  float zMin =  1.e3f;
  float zMax = -1.e3f;
  float sy, sz;
  if ( float_v::Size > 1 ) {
    float_v min = yMin;
    float_v max = yMax;
    for ( int i = clusterDataOffset; i < clusterDataOffset + row->fNHits; ++i ) {
      float tmp[4] = { data.Y( i ), data.Z( i ), 0.f, 0.f };
      float_v r;
      r.load( tmp, Vc::Unaligned);
//      const float_v r = float_v::loadUnaligned( tmp );
      //std::cout << r << std::endl;
      min = CAMath::Min( min, r );
      max = CAMath::Max( max, r );
    }
            // slide boaders apart a little
//     min -= 1.;
//     max += 1.;
    
    yMin = min[0];
    zMin = min[1];
    yMax = max[0];
    zMax = max[1];

    const float_v s = CAMath::Max( ( max - min ) * norm, float_v( 2.f ) );
    sy = s[0];
    sz = s[1];
  } else {
    for ( int i = clusterDataOffset; i < clusterDataOffset + row->fNHits; ++i ) {
      const float y = data.Y( i );
      const float z = data.Z( i );
      if ( yMax < y ) yMax = y;
      if ( yMin > y ) yMin = y;
      if ( zMax < z ) zMax = z;
      if ( zMin > z ) zMin = z;
    }
    sy = CAMath::Max( ( yMax - yMin ) * norm, 2.f );
    sz = CAMath::Max( ( zMax - zMin ) * norm, 2.f );
  }
    // slide boaders a little apart
//   yMin -= sy/2;
//   yMax += sy/2;
//   zMin -= sz/2;
//   zMax += sz/2;
//   sy = ( yMax - yMin ) * norm;
//   sz = ( zMax - zMin ) * norm;
  
  row->fGrid.Create( yMin, yMax, zMin, zMax, sy, sz );
}

void AliHLTTPCCASliceData::Clear()
{
  fNumberOfHits = 0;
}

void AliHLTTPCCASliceData::InitializeRows( const AliHLTTPCCAParam &p )
{
  fParam = &p;
  const float tan = CAMath::Tan( p.DAlpha() / 2.f );
  for ( int i = 0; i < p.NRows(); ++i ) {
    //fRows[i].fX = p.RowX( i );
    fRows[i].fMaxY = tan * p.RowX( i );
  }
}

void AliHLTTPCCASliceData::InitFromClusterData( const AliHLTTPCCAClusterData &data )
{
  ////////////////////////////////////
  // 1. prepare arrays
  ////////////////////////////////////

  fNumberOfHits = data.NumberOfClusters();

  int numberOfHitsWithPadding = 0;
  for ( int row = data.FirstRow(); row <= data.LastRow(); ++row ) {
    numberOfHitsWithPadding += NextMultipleOf<VectorAlignment>( data.NumberOfClusters( row ) );
  }

  const int numberOfRows = data.LastRow() - data.FirstRow() + 1;
  const int memorySize =
    // LinkData
    2 * numberOfHitsWithPadding * sizeof( short ) +
    // HitData
    2 * numberOfHitsWithPadding * sizeof( StoredFloat ) +
    // IsUsedData
    numberOfHitsWithPadding * sizeof( short ) +
    // FirstHitInBin
    NextMultipleOf<VectorAlignment>( ( 23 * numberOfRows + 4 * fNumberOfHits ) * sizeof( short ) ) +
    // HitWeights, ClusterDataIndex
    2 * numberOfHitsWithPadding * sizeof( int );

  if ( fMemorySize < memorySize ) {
    fMemorySize = memorySize;
    delete[] fMemory;
    fMemory = new char[fMemorySize + 7 * VectorAlignment];
  }


  short *linkUpData;
  short *linkDownData;
  StoredFloat *hitDataY;
  StoredFloat *hitDataZ;
  short *hitDataIsUsed;
  int *clusterDataIndex;
  unsigned short *hitWeights;
  unsigned short *firstHitInBin;

  char *mem = fMemory;
  AssignMemoryAligned<VectorAlignment>( linkUpData,   mem, numberOfHitsWithPadding );
  AssignMemoryAligned<VectorAlignment>( linkDownData, mem, numberOfHitsWithPadding );
  AssignMemoryAligned<VectorAlignment>( hitDataY,     mem, numberOfHitsWithPadding );
  AssignMemoryAligned<VectorAlignment>( hitDataZ,     mem, numberOfHitsWithPadding );
  AssignMemoryAligned<VectorAlignment>( hitDataIsUsed,     mem, numberOfHitsWithPadding );
  /*
   * The size of the array is row.Grid.N + row.Grid.Ny + 3. The row.Grid.Ny + 3 is an optimization
   * to remove the need for bounds checking. The last values are the same as the entry at [N - 1].
   */
  AssignMemoryAligned<VectorAlignment>( firstHitInBin,  mem, 23 * numberOfRows + 4 * fNumberOfHits );
  AssignMemoryAligned<VectorAlignment>( hitWeights,   mem, numberOfHitsWithPadding );
  AssignMemoryAligned<VectorAlignment>( clusterDataIndex, mem, numberOfHitsWithPadding );

#ifndef NVALGRIND
  ////////////////////////////////////
  // 1.5. fill HitData with 0 for valgrind
  ////////////////////////////////////

  {
    const float_v zero( Vc::Zero );
    for ( int i = 0; i < numberOfHitsWithPadding; i += float_v::Size ) {
      zero.store( &hitDataY[i] );
      zero.store( &hitDataZ[i] );
      zero.store( &hitDataIsUsed[i] );
    }
  }
#endif

  ////////////////////////////////////
  // 2. fill HitData and FirstHitInBin
  ////////////////////////////////////

  for ( int rowIndex = 0; rowIndex < data.FirstRow(); ++rowIndex ) {
    AliHLTTPCCARow &row = fRows[rowIndex];
    row.fGrid.CreateEmpty();
    row.fNHits = 0;
    row.fLinkUpData = linkUpData;
    row.fLinkDownData = linkDownData;
    row.fHitDataY = hitDataY;
    row.fHitDataZ = hitDataZ;
    row.fHitDataIsUsed = hitDataIsUsed;
    row.fClusterDataIndex = clusterDataIndex;
    row.fHitWeights = hitWeights;
    row.fFirstHitInBin = firstHitInBin;
  }

  AliHLTResizableArray<float> binSortedHitsY( fNumberOfHits );
  AliHLTResizableArray<float> binSortedHitsZ( fNumberOfHits );
  
  int gridContentOffset = 0;

  int binCreationMemorySize = 103 * 2 + fNumberOfHits;
  AliHLTResizableArray<unsigned short> binCreationMemory( binCreationMemorySize );

  int hitNumberOffset = 0;

  for ( int rowIndex = data.FirstRow(); rowIndex <= data.LastRow(); ++rowIndex ) {
    AliHLTTPCCARow &row = fRows[rowIndex];
    const int clusterDataOffset = data.RowOffset( rowIndex );

    assert( data.NumberOfClusters( rowIndex ) < ( 1 << ( sizeof( unsigned short ) * 8 - 1 ) ) );
    row.fNHits = data.NumberOfClusters( rowIndex );

    row.fLinkUpData = &linkUpData[hitNumberOffset];
    row.fLinkDownData = &linkDownData[hitNumberOffset];
    row.fHitDataY = &hitDataY[hitNumberOffset];
    row.fHitDataZ = &hitDataZ[hitNumberOffset];
    row.fHitDataIsUsed = &hitDataIsUsed[hitNumberOffset];
    row.fClusterDataIndex = &clusterDataIndex[hitNumberOffset];
    row.fHitWeights = &hitWeights[hitNumberOffset];
    row.fHitNumberOffset = hitNumberOffset;

    row.fFirstHitInBin = &firstHitInBin[gridContentOffset];

    createGrid( &row, data, clusterDataOffset );
    const AliHLTTPCCAGrid &grid = row.fGrid;
    const int numberOfBins = grid.N();

    int binCreationMemorySizeNew;
    if ( ( binCreationMemorySizeNew = numberOfBins * 2 + 6 + row.fNHits ) > binCreationMemorySize ) {
      binCreationMemorySize = binCreationMemorySizeNew;
      binCreationMemory.Resize( binCreationMemorySize );
    }

    AliHLTArray<unsigned short> c = binCreationMemory;           // number of hits in all previous bins
    AliHLTArray<unsigned short> bins = c + ( numberOfBins + 3 ); // cache for the bin index for every hit in this row
    AliHLTArray<unsigned short> filled = bins + row.fNHits;      // counts how many hits there are per bin

    for ( unsigned int bin = 0; bin < row.fGrid.N() + 3; ++bin ) {
      filled[bin] = 0; // initialize filled[] to 0
    }

    for ( int hitIndex = 0; hitIndex < row.fNHits; ++hitIndex ) {
      const int globalHitIndex = clusterDataOffset + hitIndex;
      const unsigned short bin = row.fGrid.GetBin( data.Y( globalHitIndex ), data.Z( globalHitIndex ) );
      bins[hitIndex] = bin;
      ++filled[bin];
    }

    unsigned short n = 0;
    for ( int bin = 0; bin < numberOfBins + 3; ++bin ) {
      c[bin] = n;
      n += filled[bin];
    }

    for ( int hitIndex = 0; hitIndex < row.fNHits; ++hitIndex ) {
      const unsigned short bin = bins[hitIndex];
#ifndef NVALGRIND
      VALGRIND_CHECK_VALUE_IS_DEFINED( bin );
#endif
      assert( bin < numberOfBins + 3 );
      --filled[bin];
      const unsigned short ind = c[bin] + filled[bin]; // generate an index for this hit that is >= c[bin] and < c[bin + 1]
      assert( ind < row.fNHits );
#ifndef NVALGRIND
      VALGRIND_CHECK_VALUE_IS_DEFINED( ind );
#endif
      const int globalHitIndex = clusterDataOffset + hitIndex;

      // allows to find the global hit index / coordinates from a global bin sorted hit index
#ifndef NVALGRIND
      VALGRIND_CHECK_VALUE_IS_DEFINED( globalHitIndex );
#endif
      row.fClusterDataIndex[ind] = globalHitIndex;
      row.fHitDataY[ind] = data.Y( globalHitIndex );
      row.fHitDataZ[ind] = data.Z( globalHitIndex );
      row.fHitDataIsUsed[ind] = 0;
    }

    for ( int i = 0; i < numberOfBins; ++i ) {
      row.fFirstHitInBin[i] = c[i]; // global bin-sorted hit index
    }
    const unsigned short a = c[numberOfBins];
    // grid.N is <= row.fNHits
    const int nn = numberOfBins + grid.Ny() + 3;
    assert( static_cast<int>( gridContentOffset ) + nn - 1 < 23 * numberOfRows + 4 * fNumberOfHits );
    for ( int i = numberOfBins; i < nn; ++i ) {
      row.fFirstHitInBin[i] = a;
    }

    gridContentOffset += nn;
    hitNumberOffset += NextMultipleOf<VectorAlignment>( row.fNHits );
  }

  for ( int rowIndex = data.LastRow() + 1; rowIndex < AliHLTTPCCAParameters::MaxNumberOfRows8; ++rowIndex ) {
    AliHLTTPCCARow &row = fRows[rowIndex];
    row.fGrid.CreateEmpty();
    row.fNHits = 0;
    row.fLinkUpData = &linkUpData[hitNumberOffset];
    row.fLinkDownData = &linkDownData[hitNumberOffset];
    row.fHitDataY = &hitDataY[hitNumberOffset];
    row.fHitDataZ = &hitDataZ[hitNumberOffset];
    row.fHitDataIsUsed = &hitDataIsUsed[hitNumberOffset];
    row.fClusterDataIndex = &clusterDataIndex[hitNumberOffset];
    row.fHitWeights = &hitWeights[hitNumberOffset];
    row.fFirstHitInBin = &firstHitInBin[gridContentOffset];
  }
}

void AliHLTTPCCASliceData::ClearHitWeights()
{
  const ushort_v v0( Vc::Zero );
  const unsigned short *const end = fRows[fParam->NRows()].fHitWeights;
  for ( unsigned short *mem = fRows[0].fHitWeights; mem < end; mem += v0.Size ) {
    v0.store( mem );
  }
}

void AliHLTTPCCASliceData::ClearLinks()
{
  const short_v v0( -1 );
  const short *const end1 = fRows[fParam->NRows()].fLinkUpData;
  for ( short *mem = fRows[0].fLinkUpData; mem < end1; mem += v0.Size ) {
    v0.store( mem );
  }
  const short *const end2 = fRows[fParam->NRows()].fLinkDownData;
  for ( short *mem = fRows[0].fLinkDownData; mem < end2; mem += v0.Size ) {
    v0.store( mem );
  }
}

///////////////////////
// for debugging
///////////////////////

#include "BinaryStoreHelper.h"

void AliHLTTPCCASliceData::StoreToFile( FILE *f ) const
{
  BinaryStoreWrite( fNumberOfHits, f );

  const int size = fMemorySize;
  assert( size >= 0 );
  assert( size <= fMemorySize );
  BinaryStoreWrite( size, f );
  BinaryStoreWrite( static_cast<Byte_t>( reinterpret_cast<unsigned long>( fMemory ) & 0xff ), f );
  BinaryStoreWrite( fMemory, size, f );

  for ( int i = 0; i < 200; ++i ) {
    fRows[i].StoreToFile( f, fMemory );
  }
}

void AliHLTTPCCASliceData::RestoreFromFile( FILE *f )
{
  BinaryStoreRead( fNumberOfHits, f );

  BinaryStoreRead( fMemorySize, f );
  Byte_t alignment;
  BinaryStoreRead( alignment, f );
  fMemory = new char[fMemorySize + 256];
  const Byte_t offset = alignment - static_cast<Byte_t>( reinterpret_cast<unsigned long>( fMemory ) & 0xff );
  BinaryStoreRead( fMemory + offset, fMemorySize, f );

  for ( int i = 0; i < 200; ++i ) {
    fRows[i].RestoreFromFile( f, fMemory + offset );
  }
}
