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

#ifndef SLICEDATAVECTOR_H
#define SLICEDATAVECTOR_H

#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAClusterData.h"
#include <cstdio>

#include "debug.h"

class AliHLTTPCCAClusterData;
class AliHLTTPCCAHit;
class AliHLTTPCCAParam;

// index types. For efficient loads and stores we want only neighbours, so the index is a scalar
// giving the index of the first item. How many items are actually returned is determined by the
// size of the vector that is returned (e.g. short_v::Size or float_v::Size).
typedef short short_i;
typedef unsigned int uint_i;

/**
 * Data abstraction class for the Slice Tracker.
 *
 * Different architectures implement this for the most efficient loads and stores. All access to the
 * data happens through inline functions so that access to the data has no extra costs.
 */
class AliHLTTPCCASliceData
{
  public:
    AliHLTTPCCASliceData() : fRows(), fNumberOfHits(0), fMemorySize( 0 ), fMemory( 0 ), fParam( 0 ) {}
    ~AliHLTTPCCASliceData() { if (fMemory) delete[] fMemory; }

    void InitializeRows( const AliHLTTPCCAParam &parameters );

    /**
     * (Re)Create the data that is tuned for optimal performance of the algorithm from the cluster
     * data.
     */
    void InitFromClusterData( const AliHLTTPCCAClusterData &data );

    /**
     * Clear the slice data (e.g. for an empty slice)
     */
    void Clear();

    /**
     * Return the number of hits in this slice.
     */
    int NumberOfHits() const { return fNumberOfHits; }

    /**
     * Access to the hit links.
     *
     * The links values give the hit index in the row above/below. Or -1 if there is no link.
     */
    short HitLinkUpDataS  ( const AliHLTTPCCARow &row, short hitIndex ) const;
    short HitLinkDownDataS( const AliHLTTPCCARow &row, short hitIndex ) const;
    const short *HitLinkUpData  ( const AliHLTTPCCARow &row ) const;
    const short *HitLinkDownData( const AliHLTTPCCARow &row ) const;
    short_v HitLinkUpData  ( const AliHLTTPCCARow &row, const short_i &hitIndex ) const;
    short_v HitLinkDownData( const AliHLTTPCCARow &row, const short_i &hitIndex ) const;
    short_v HitLinkUpData  ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes ) const;
    short_v HitLinkDownData( const AliHLTTPCCARow &row, const ushort_v &hitIndexes ) const;
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const short_i &hitIndex, const short_v &value );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const short_i &hitIndex, const short_v &value );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value, const short_m &mask );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value, const short_m &mask );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value);

    /**
     * Reset all links to -1.
     */
    void ClearLinks();

    /**
     * Return the y and z coordinate(s) of the given hit(s).
     */
    float HitDataXS( const AliHLTTPCCARow &row, short hitIndex ) const;
    float HitDataYS( const AliHLTTPCCARow &row, short hitIndex ) const;
    float HitDataZS( const AliHLTTPCCARow &row, short hitIndex ) const;
    const float *HitDataY( const AliHLTTPCCARow &row ) const;
    const float *HitDataZ( const AliHLTTPCCARow &row ) const;
    const short *HitDataIsUsed( const AliHLTTPCCARow &row ) const;
    sfloat_v HitDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    sfloat_v HitDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    sfloat_v HitDataY( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const sfloat_m &mask ) const;
    sfloat_v HitDataZ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const sfloat_m &mask ) const;
    //short_v  HitDataIsUsed( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    void SetHitAsUsed( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_m &mask );
    void SetHitAsUsedInTrackFit( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_m &mask );
    void SetHitAsUsedInTrackExtend( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_m &mask );
  
    /**
     * For a given bin index, content tells how many hits there are in the preceding bins. This maps
     * directly to the hit index in the given row.
     *
     * \param binIndexes in the range 0 to row.Grid.N + row.Grid.Ny + 3.
     */
    ushort_v FirstHitInBin( const AliHLTTPCCARow &row, ushort_v binIndexes ) const;
    const unsigned short *FirstHitInBin( const AliHLTTPCCARow &row ) const;
    unsigned short FirstHitInBin( const AliHLTTPCCARow &row, unsigned short binIndex ) const;

    /**
     * The hit weight is used to determine whether a hit belongs to a certain tracklet or another one
     * competing for the same hit. The tracklet that has a higher weight wins.  Comparison is done
     * using the the number of hits in the tracklet ( the more hits it has the more it keeps ). If
     * tracklets have the same number of hits then it doesn't matter who gets it, but it should be
     * only one. So a unique number ( row index is good ) is added in the least significant part of
     * the weight
     */
    static ushort_v CalculateHitWeight( ushort_v numberOfHits, ushort_v unique );

    ushort_m TakeOwnHits( const AliHLTTPCCARow &row, const ushort_v &hitIndex, const ushort_m &mask,
        const ushort_v &weights ) const;

    /**
     * If the given weight is higher than what is currently stored replace with the new weight.
     */
    void MaximizeHitWeight( const AliHLTTPCCARow &row, const ushort_v &hitIndex,
        const ushort_v &weight );

    /**
     * Return the maximal weight the given hit got from one tracklet
     */
    ushort_v HitWeight( const AliHLTTPCCARow &row, const ushort_v &hitIndex, const ushort_m &mask ) const;

    /**
     * Reset all hit weights to 0.
     */
    void ClearHitWeights();

    /**
     * Returns the index in the original AliHLTTPCCAClusterData object of the given hit
     */
    int ClusterDataIndex( const AliHLTTPCCARow &row, int hitIndex ) const;

    /**
     * Return the row object for the given row index.
     */
    const AliHLTTPCCARow &Row( int rowIndex ) const;

    // for debugging:
    void StoreToFile( FILE * ) const;
    void RestoreFromFile( FILE * );

    const float *RowX() const { return fParam->RowX(); }
    float RowX( int i ) const { return fParam->RowX( i ); }

  private:
    enum {
      VectorSizeFactor = ushort_v::Size / float_v::Size
    };

    void createGrid( AliHLTTPCCARow *row, const AliHLTTPCCAClusterData &data, const int clusterDataOffset );

    AliHLTFixedArray<AliHLTTPCCARow, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8+1>, AliHLTFullyCacheLineAligned> fRows; // The row objects needed for most accessor functions

    int fNumberOfHits;         // the number of hits in this slice
    int fMemorySize;           // size of the allocated memory in bytes
    char *fMemory;             // pointer to the allocated memory where all the following arrays reside in
    const AliHLTTPCCAParam *fParam;  // pointer to the Param object for gathering X coordinates of rows
};

inline short AliHLTTPCCASliceData::HitLinkUpDataS  ( const AliHLTTPCCARow &row, short hitIndex ) const
{
  return row.fLinkUpData[hitIndex];
}

inline short AliHLTTPCCASliceData::HitLinkDownDataS( const AliHLTTPCCARow &row, short hitIndex ) const
{
  return row.fLinkDownData[hitIndex];
}

inline const short *AliHLTTPCCASliceData::HitLinkUpData  ( const AliHLTTPCCARow &row ) const
{
  return row.fLinkUpData;
}

inline const short *AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row ) const
{
  return row.fLinkDownData;
}

inline short_v AliHLTTPCCASliceData::HitLinkUpData  ( const AliHLTTPCCARow &row, const short_i &hitIndex ) const
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( short_v::EntryType ) % VectorAlignment == 0 );
  return short_v( &row.fLinkUpData[hitIndex] );
}

inline short_v AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row, const short_i &hitIndex ) const
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( short_v::EntryType ) % VectorAlignment == 0 );
  return short_v( &row.fLinkDownData[hitIndex] );
}

inline short_v AliHLTTPCCASliceData::HitLinkUpData  ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes ) const
{
  return short_v( row.fLinkUpData, hitIndexes );
}

inline short_v AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row, const ushort_v &hitIndexes ) const
{
  //   short_v tmp; // IKu debug
//   tmp.gather( row.fLinkDownData, hitIndexes );
//   return tmp;
//   short *array = row.fLinkDownData; // IKu debug
//   for (int i = 0; i < short_v::Size; i++){
//     std::cout << array[hitIndexes[i]] << " ";
//   }
//   std::cout << std::endl;
  return short_v( row.fLinkDownData, hitIndexes );
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const short_i &hitIndex, const short_v &value )
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( short_v::EntryType ) % VectorAlignment == 0 );
  value.store( &row.fLinkUpData[hitIndex] );
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const short_i &hitIndex, const short_v &value )
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( short_v::EntryType ) % VectorAlignment == 0 );
  value.store( &row.fLinkDownData[hitIndex] );
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value, const short_m &mask )
{ 
//   value.scatter( row.fLinkUpData, hitIndexes, mask );
  short *array = row.fLinkUpData;
  foreach_bit(int i, mask){
    array[hitIndexes[i]] = value[i];
  }
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value, const short_m &mask )
{ 
   // value.scatter( row.fLinkDownData, hitIndexes, mask );
  short *array = row.fLinkDownData;
  foreach_bit(int i, mask){
    array[hitIndexes[i]] = value[i];
  }
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value)
{
   // value.scatter( row.fLinkUpData, hitIndexes);
  short *array = row.fLinkUpData;
  for (int i = 0; i < short_v::Size; i++){
    array[hitIndexes[i]] = value[i];
  }
  
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_v &value)
{
//   value.scatter( row.fLinkDownData, hitIndexes);
  short *array = row.fLinkDownData;
  for (int i = 0; i < short_v::Size; i++){
    array[hitIndexes[i]] = value[i];
  }
}

inline float AliHLTTPCCASliceData::HitDataXS( const AliHLTTPCCARow &row, short hitIndex ) const
{// TODO read from hits-file
  hitIndex++;
  int i = 0;
  for( ; i < fRows.Size(); i++ ) {
    if (fRows[i].fHitDataZ == row.fHitDataZ) break;
  }
  return RowX(i); 
}

inline float AliHLTTPCCASliceData::HitDataYS( const AliHLTTPCCARow &row, short hitIndex ) const
{
  return row.fHitDataY[hitIndex];
}

inline float AliHLTTPCCASliceData::HitDataZS( const AliHLTTPCCARow &row, short hitIndex ) const
{
  return row.fHitDataZ[hitIndex];
}

inline const float *AliHLTTPCCASliceData::HitDataY( const AliHLTTPCCARow &row ) const
{
  return row.fHitDataY;
}

inline const float *AliHLTTPCCASliceData::HitDataZ( const AliHLTTPCCARow &row ) const
{
  return row.fHitDataZ;
}

inline const short *AliHLTTPCCASliceData::HitDataIsUsed( const AliHLTTPCCARow &row ) const
{
  return row.fHitDataIsUsed;
}

inline void AliHLTTPCCASliceData::SetHitAsUsed( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_m &mask )
{
  //short_v(Vc::One).scatter( row.fHitDataIsUsed, static_cast<ushort_v>(hitIndexes), mask); // TODO why don't work w\o cast????
  short *array = row.fHitDataIsUsed; // TODO don't work with scatter!
  foreach_bit(int i, mask){
    array[hitIndexes[i]] = 1;
  }
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInTrackFit( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_m &mask )
{
  // short_v(2).scatter( row.fHitDataIsUsed, static_cast<ushort_v>(hitIndexes), mask);
  short *array = row.fHitDataIsUsed;
  foreach_bit(int i, mask){
    array[hitIndexes[i]] = 2;
  }
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInTrackExtend( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const short_m &mask )
{
  // short_v(3).scatter( row.fHitDataIsUsed, static_cast<ushort_v>(hitIndexes), mask);
  short *array = row.fHitDataIsUsed;
  foreach_bit(int i, mask){
    array[hitIndexes[i]] = 3;
  }
}

inline sfloat_v AliHLTTPCCASliceData::HitDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( sfloat_v::EntryType ) % VectorAlignment == 0 );
  return sfloat_v( &row.fHitDataY[hitIndex] );
}

inline sfloat_v AliHLTTPCCASliceData::HitDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( sfloat_v::EntryType ) % VectorAlignment == 0 );
  return sfloat_v( &row.fHitDataZ[hitIndex] );
}

inline sfloat_v AliHLTTPCCASliceData::HitDataY( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const sfloat_m &mask ) const
{
  return sfloat_v( row.fHitDataY, hitIndexes, mask );
}

inline sfloat_v AliHLTTPCCASliceData::HitDataZ( const AliHLTTPCCARow &row, const ushort_v &hitIndexes, const sfloat_m &mask ) const
{
  return sfloat_v( row.fHitDataZ, hitIndexes, mask );
}

// inline short_v AliHLTTPCCASliceData::HitDataIsUsed( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
// { // don't really need this
//   return short_v( &row.fHitDataIsUsed[hitIndex] );
// }

inline ushort_v AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row, ushort_v binIndexes ) const
{
  const ushort_v tmp( row.fFirstHitInBin, binIndexes );
  assert( tmp <= row.NHits() );
  return tmp;
}

inline const unsigned short *AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row ) const
{
  return row.fFirstHitInBin;
}

inline unsigned short AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row, unsigned short binIndex ) const
{
  return row.fFirstHitInBin[binIndex];
}

inline int AliHLTTPCCASliceData::ClusterDataIndex( const AliHLTTPCCARow &row, int hitIndex ) const
{
  return row.fClusterDataIndex[hitIndex];
}

inline const AliHLTTPCCARow &AliHLTTPCCASliceData::Row( int rowIndex ) const
{
  return fRows[rowIndex];
}

inline ushort_v AliHLTTPCCASliceData::CalculateHitWeight( ushort_v numberOfHits, ushort_v unique )
{
//  return ( numberOfHits << 8 ) | ( unique & 0xff ); // CHECKME we don't need this unique?
  return ( numberOfHits << 8 ) | ( unique & 0 );
}

inline ushort_m AliHLTTPCCASliceData::TakeOwnHits( const AliHLTTPCCARow &row,
    const ushort_v &hitIndex, const ushort_m &mask, const ushort_v &weights ) const
{
  const ushort_v storedWeights( row.fHitWeights, hitIndex, mask );
  const ushort_m own = storedWeights == weights && mask;
  const ushort_v takenMarker = std::numeric_limits<ushort_v>::max();
  takenMarker.scatter( row.fHitWeights, hitIndex, own );
  return own;
}

inline void AliHLTTPCCASliceData::MaximizeHitWeight( const AliHLTTPCCARow &row,
    const ushort_v &hitIndex, const ushort_v &weight )
{
  const short_m mask = validHitIndexes( hitIndex );
  // XXX critical section if the TrackletConstructor gets multi-threaded
  const ushort_v oldWeight( row.fHitWeights, hitIndex, mask );
  debugF() << "scatter HitWeigths " << weight << " to " << hitIndex << ( weight > oldWeight && mask ) << " old: " << oldWeight << std::endl;
  weight.scatter( row.fHitWeights, hitIndex, weight > oldWeight && mask );
}

inline ushort_v AliHLTTPCCASliceData::HitWeight( const AliHLTTPCCARow &row, const ushort_v &hitIndex, const ushort_m &mask ) const
{
  return ushort_v( row.fHitWeights, hitIndex, mask );
}


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
  const float minCellSize = AliHLTTPCCAParameters::MinCellSize;
  if ( row->NHits() <= 0 ) { // no hits or invalid data
    // grid coordinates don't matter, since there are no hits
    row->fGrid.CreateEmpty();
    return;
  } else if ( row->NHits() == 1 ) {
    const float y = data.Y( clusterDataOffset );
    const float z = data.Z( clusterDataOffset );
    row->fGrid.Create1( y, z, minCellSize, minCellSize );
    return;
  }

  const float norm = fastInvSqrt( AliHLTTPCCAParameters::GridCreationCoeff*row->fNHits ); // TODO parameter
//  const float norm = 1./sqrt( 5*row->fNHits );

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

    const float_v s = CAMath::Max( ( max - min ) * norm, float_v( minCellSize ) );
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
    sy = CAMath::Max( ( yMax - yMin ) * norm, minCellSize );
    sz = CAMath::Max( ( zMax - zMin ) * norm, minCellSize );
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


typedef AliHLTTPCCASliceData SliceData;

#endif // SLICEDATAVECTOR_H
