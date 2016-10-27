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
// size of the vector that is returned (e.g. int_v::Size or float_v::Size).
typedef int int_i;
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
    AliHLTTPCCASliceData() : fMemorySize( 0 ), fMemory( 0 ), fParam( 0 ) {}
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
    int HitLinkUpDataS  ( const AliHLTTPCCARow &row, int hitIndex ) const;
    int HitLinkDownDataS( const AliHLTTPCCARow &row, int hitIndex ) const;
    const int *HitLinkUpData  ( const AliHLTTPCCARow &row ) const;
    const int *HitLinkDownData( const AliHLTTPCCARow &row ) const;
    int_v HitLinkUpData  ( const AliHLTTPCCARow &row, const int_i &hitIndex ) const;
    int_v HitLinkDownData( const AliHLTTPCCARow &row, const int_i &hitIndex ) const;
    int_v HitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes ) const;
    int_v HitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes ) const;
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value);
    void SetUnusedHitLinkUpData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowUp, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetUnusedHitLinkDownData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowDn, const uint_v &hitIndexes, const int_v &value, const int_m &mask );

    /**
     * Reset all links to -1.
     */
    void ClearLinks();

    /**
     * Return the y and z coordinate(s) of the given hit(s).
     */
    float HitDataXS( const AliHLTTPCCARow &row, int hitIndex ) const;
    // float HitDataYS( const AliHLTTPCCARow &row, int hitIndex ) const;
    // float HitDataZS( const AliHLTTPCCARow &row, int hitIndex ) const;
    float HitPDataYS( const AliHLTTPCCARow &row, int hitIndex ) const;
    float HitPDataZS( const AliHLTTPCCARow &row, int hitIndex ) const;

    // const float *HitDataY( const AliHLTTPCCARow &row ) const;
    // const float *HitDataZ( const AliHLTTPCCARow &row ) const;
    const int *HitDataIsUsed( const AliHLTTPCCARow &row ) const;
    // float_v HitDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    // float_v HitDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    // float_v HitDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    // float_v HitDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v HitPDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    float_v HitPDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    float_v HitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v HitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v UnusedHitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v UnusedHitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;

    //int_v  HitDataIsUsed( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    void SetHitAsUsed( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );
    void SetHitAsUsedInTrackFit( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );
    void SetHitAsUsedInTrackExtend( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );

    void CleanUsedHits( int rowIndex, bool isFirst );
  
    /**
     * For a given bin index, content tells how many hits there are in the preceding bins. This maps
     * directly to the hit index in the given row.
     *
     * \param binIndexes in the range 0 to row.Grid.N + row.Grid.Ny + 3.
     */
    uint_v FirstHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const;
    const unsigned int *FirstHitInBin( const AliHLTTPCCARow &row ) const;
    unsigned int FirstHitInBin( const AliHLTTPCCARow &row, unsigned int binIndex ) const;
    uint_v FirstUnusedHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const;
    const unsigned int *FirstUnusedHitInBin( const AliHLTTPCCARow &row ) const;

    /**
     * The hit weight is used to determine whether a hit belongs to a certain tracklet or another one
     * competing for the same hit. The tracklet that has a higher weight wins.  Comparison is done
     * using the the number of hits in the tracklet ( the more hits it has the more it keeps ). If
     * tracklets have the same number of hits then it doesn't matter who gets it, but it should be
     * only one. So a unique number ( row index is good ) is added in the least significant part of
     * the weight
     */
    static uint_v CalculateHitWeight( uint_v numberOfHits, uint_v unique );

    uint_m TakeOwnHits( const AliHLTTPCCARow &row, const uint_v &hitIndex, const uint_m &mask,
        const uint_v &weights ) const;

    /**
     * If the given weight is higher than what is currently stored replace with the new weight.
     */
    void MaximizeHitWeight( const AliHLTTPCCARow &row, const uint_v &hitIndex,
        const uint_v &weight );

    /**
     * Return the maximal weight the given hit got from one tracklet
     */
    uint_v HitWeight( const AliHLTTPCCARow &row, const uint_v &hitIndex, const uint_m &mask ) const;

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
      VectorSizeFactor = uint_v::Size / float_v::Size
    };

    void createGrid( AliHLTTPCCARow *row, const AliHLTTPCCAClusterData &data, const int clusterDataOffset );

    AliHLTFixedArray<AliHLTTPCCARow, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8+1>, AliHLTFullyCacheLineAligned> fRows; // The row objects needed for most accessor functions

    int fNumberOfHits;         // the number of hits in this slice
    int fMemorySize;           // size of the allocated memory in bytes
    char *fMemory;             // pointer to the allocated memory where all the following arrays reside in
    const AliHLTTPCCAParam *fParam;  // pointer to the Param object for gathering X coordinates of rows
};

inline int AliHLTTPCCASliceData::HitLinkUpDataS  ( const AliHLTTPCCARow &row, int hitIndex ) const
{
  return row.fLinkUpData[hitIndex];
}

inline int AliHLTTPCCASliceData::HitLinkDownDataS( const AliHLTTPCCARow &row, int hitIndex ) const
{
  return row.fLinkDownData[hitIndex];
}

inline const int *AliHLTTPCCASliceData::HitLinkUpData  ( const AliHLTTPCCARow &row ) const
{
  return row.fLinkUpData;
}

inline const int *AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row ) const
{
  return row.fLinkDownData;
}

inline int_v AliHLTTPCCASliceData::HitLinkUpData  ( const AliHLTTPCCARow &row, const int_i &hitIndex ) const
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( int_v::EntryType ) % VectorAlignment == 0 );
  return int_v( &row.fLinkUpData[hitIndex] );
}

inline int_v AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row, const int_i &hitIndex ) const
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( int_v::EntryType ) % VectorAlignment == 0 );
  return int_v( &row.fLinkDownData[hitIndex] );
}

inline int_v AliHLTTPCCASliceData::HitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes ) const
{
  return int_v( row.fLinkUpData, hitIndexes );
}

inline int_v AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes ) const
{
  //   int_v tmp; // IKu debug
//   tmp.gather( row.fLinkDownData, hitIndexes );
//   return tmp;
//   int *array = row.fLinkDownData; // IKu debug
//   for (int i = 0; i < int_v::Size; i++){
//     std::cout << array[hitIndexes[i]] << " ";
//   }
//   std::cout << std::endl;
  return int_v( row.fLinkDownData, hitIndexes );
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value )
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( int_v::EntryType ) % VectorAlignment == 0 );
  value.store( &row.fLinkUpData[hitIndex] );
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value )
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( int_v::EntryType ) % VectorAlignment == 0 );
  value.store( &row.fLinkDownData[hitIndex] );
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{ 
   value.scatter( row.fLinkUpData, hitIndexes, mask );
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{ 
  value.scatter( row.fLinkDownData, hitIndexes, mask );
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value)
{
  value.scatter( row.fLinkUpData, hitIndexes);
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value)
{
  value.scatter( row.fLinkDownData, hitIndexes);
}

inline void AliHLTTPCCASliceData::SetUnusedHitLinkUpData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowUp, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{
  const uint_v ind( row.fHitIndex, hitIndexes, mask );
  uint_v val(rowUp.fHitIndex, static_cast<uint_v>(value), mask && value != -1);
  int_v val2(val);
  val2(value == -1) = value;
  val2.scatter( row.fLinkUpData, ind, mask );
}

inline void AliHLTTPCCASliceData::SetUnusedHitLinkDownData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowDn, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{
  const uint_v ind( row.fHitIndex, hitIndexes, mask );
  uint_v val(rowDn.fHitIndex, static_cast<uint_v>(value), mask && value != -1);
  int_v val2(val);
  val2(value == -1) = value;
  val2.scatter( row.fLinkDownData, ind, mask );
}



inline float AliHLTTPCCASliceData::HitDataXS( const AliHLTTPCCARow &row, int hitIndex ) const
{// TODO read from hits-file
  hitIndex++;
  int i = 0;
  for( ; i < fRows.Size(); i++ ) {
    if (fRows[i].fHitPDataZ == row.fHitPDataZ) break;
  }
  return RowX(i); 
}

// inline float AliHLTTPCCASliceData::HitDataYS( const AliHLTTPCCARow &row, int hitIndex ) const
// {
//   return row.fHitDataY[hitIndex];
// }

// inline float AliHLTTPCCASliceData::HitDataZS( const AliHLTTPCCARow &row, int hitIndex ) const
// {
//   return row.fHitDataZ[hitIndex];
// }

// inline const float *AliHLTTPCCASliceData::HitDataY( const AliHLTTPCCARow &row ) const
// {
//   return row.fHitDataY;
// }

// inline const float *AliHLTTPCCASliceData::HitDataZ( const AliHLTTPCCARow &row ) const
// {
//   return row.fHitDataZ;
// }

inline float AliHLTTPCCASliceData::HitPDataYS( const AliHLTTPCCARow &row, int hitIndex ) const
{
  return row.fHitPDataY[hitIndex];
}

inline float AliHLTTPCCASliceData::HitPDataZS( const AliHLTTPCCARow &row, int hitIndex ) const
{
  return row.fHitPDataZ[hitIndex];
}

inline const int *AliHLTTPCCASliceData::HitDataIsUsed( const AliHLTTPCCARow &row ) const
{
  return row.fHitDataIsUsed;
}

inline void AliHLTTPCCASliceData::SetHitAsUsed( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask )
{
  int_v(Vc::One).scatter( row.fHitDataIsUsed, static_cast<uint_v>(hitIndexes), mask); // TODO why don't work w\o cast????
  // int *array = row.fHitDataIsUsed; // TODO don't work with scatter!
  // foreach_bit(int i, mask){
  //   array[hitIndexes[i]] = 1;
  // }
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInTrackFit( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask )
{
  int_v(2).scatter( row.fHitDataIsUsed, static_cast<uint_v>(hitIndexes), mask);
  // int *array = row.fHitDataIsUsed;
  // foreach_bit(int i, mask){
  //   array[hitIndexes[i]] = 2;
  // }
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInTrackExtend( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask )
{
  int_v(3).scatter( row.fHitDataIsUsed, static_cast<uint_v>(hitIndexes), mask);
  // int *array = row.fHitDataIsUsed;
  // foreach_bit(int i, mask){
  //   array[hitIndexes[i]] = 3;
  // }
}

inline void AliHLTTPCCASliceData::CleanUsedHits( int rowIndex, bool isFirst )
{
  AliHLTTPCCARow &row = fRows[rowIndex];
  const unsigned int numberOfHits = row.NHits();
  if( numberOfHits < 1 ) return;

  const unsigned int NFirstHitInBin = row.Grid().N() + row.Grid().Ny() + 3;
  if ( isFirst ) {
    memcpy( row.fUnusedHitPDataY, row.fHitPDataY, numberOfHits * sizeof(PackHelper::TPackedY) );
    memcpy( row.fUnusedHitPDataZ, row.fHitPDataZ, numberOfHits * sizeof(PackHelper::TPackedY) );
    memcpy( row.fFirstUnusedHitInBin, row.fFirstHitInBin, NFirstHitInBin * sizeof( unsigned int )  );
    for ( unsigned int i = 0; i < numberOfHits; i += uint_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      const int_m validHitsMask = (hitIndexes < numberOfHits);
      hitIndexes.scatter( row.fHitIndex, hitIndexes, validHitsMask );
    }
    row.fNUnusedHits = numberOfHits;
  }
  else {
    std::vector<unsigned int> unusedHitIndex(numberOfHits+1,numberOfHits);
    unsigned int iUH = 0;
    for ( unsigned int i = 0; i < numberOfHits; i += uint_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      const int_m validHitsMask = (hitIndexes < numberOfHits)
        && ( int_v( HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes) ) == int_v( Vc::Zero ) );
      for(unsigned int iV=0; iV<int_v::Size; iV++)
      {
        if(!validHitsMask[iV]) continue;
//         foreach_bit( int iV, validHitsMask ) {
        row.fUnusedHitPDataY[iUH] = row.fHitPDataY[hitIndexes[iV]];
        row.fUnusedHitPDataZ[iUH] = row.fHitPDataZ[hitIndexes[iV]];
        row.fHitIndex[iUH] = hitIndexes[iV];
        unusedHitIndex[hitIndexes[iV]] = iUH;
        iUH++;
      }
    }
    row.fNUnusedHits = iUH;
    unusedHitIndex[numberOfHits] = iUH;
    { // fill unfilled elements such so firstHitInBin will be correctly filled afterwards
      unsigned int last = iUH;
      for ( int i = int(unusedHitIndex.size()-2); i >= 0; i-- ) {
        if ( unusedHitIndex[i] == numberOfHits )
          unusedHitIndex[i] = last;
        else
          last = unusedHitIndex[i];
      }
    }
  
    for ( unsigned int i = 0; i < NFirstHitInBin; i ++ ) {
      assert( row.fFirstHitInBin[i] < numberOfHits + 1 );
      row.fFirstUnusedHitInBin[i] = unusedHitIndex[row.fFirstHitInBin[i]];
    }
  }
}

// inline float_v AliHLTTPCCASliceData::HitDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
// {
//   //Matthias 01.24.13  assert( hitIndex * sizeof( float_v::EntryType ) % VectorAlignment == 0 );
//   return float_v( &row.fHitDataY[hitIndex] );
// }

// inline float_v AliHLTTPCCASliceData::HitDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
// {
//   //Matthias 01.24.13  assert( hitIndex * sizeof( float_v::EntryType ) % VectorAlignment == 0 );
//   return float_v( &row.fHitDataZ[hitIndex] );
// }

// inline float_v AliHLTTPCCASliceData::HitDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
// {
//   return float_v( row.fHitDataY, hitIndexes, mask );
// }

// inline float_v AliHLTTPCCASliceData::HitDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
// {
//   return float_v( row.fHitDataZ, hitIndexes, mask );
// }

inline float_v AliHLTTPCCASliceData::HitPDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
{
  PackHelper::TPackedY_v r;
  for(unsigned  int i = 0; i < float_v::Size; i++){ 
    r[i] = row.fHitPDataY[hitIndex + i];
  }
  return PackHelper::UnpackY( row, r );
}

inline float_v AliHLTTPCCASliceData::HitPDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
{
  PackHelper::TPackedZ_v r;
  for(unsigned  int i = 0; i < float_v::Size; i++){ 
    r[i] = row.fHitPDataZ[hitIndex + i];
  }
  return PackHelper::UnpackZ( row, r );
}

inline float_v AliHLTTPCCASliceData::HitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  PackHelper::TPackedY_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
//   foreach_bit(int i, mask){ 
    r[i] = row.fHitPDataY[hitIndexes[i]];
  }
  return PackHelper::UnpackY( row, r );
}

inline float_v AliHLTTPCCASliceData::HitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  PackHelper::TPackedZ_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
//   foreach_bit(int i, mask){ 
    r[i] = row.fHitPDataZ[hitIndexes[i]];
  }
  return PackHelper::UnpackZ( row, r );
}

inline float_v AliHLTTPCCASliceData::UnusedHitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  PackHelper::TPackedY_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
//   foreach_bit(int i, mask){ 
    r[i] = row.fUnusedHitPDataY[hitIndexes[i]];
  }
  return PackHelper::UnpackY( row, r );
}

inline float_v AliHLTTPCCASliceData::UnusedHitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  PackHelper::TPackedZ_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
//   foreach_bit(int i, mask){ 
    r[i] = row.fUnusedHitPDataZ[hitIndexes[i]];
  }
  return PackHelper::UnpackZ( row, r );
}

// inline int_v AliHLTTPCCASliceData::HitDataIsUsed( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
// { // don't really need this
//   return int_v( &row.fHitDataIsUsed[hitIndex] );
// }

inline uint_v AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const
{
  const uint_v tmp( row.fFirstHitInBin, binIndexes );
  return tmp;
}

inline const unsigned int *AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row ) const
{
  return row.fFirstHitInBin;
}

inline unsigned int AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row, unsigned int binIndex ) const
{
  return row.fFirstHitInBin[binIndex];
}

inline uint_v AliHLTTPCCASliceData::FirstUnusedHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const
{
  const uint_v tmp( row.fFirstUnusedHitInBin, binIndexes );
  return tmp;
}

inline const unsigned int *AliHLTTPCCASliceData::FirstUnusedHitInBin( const AliHLTTPCCARow &row ) const
{
  return row.fFirstUnusedHitInBin;
}

inline int AliHLTTPCCASliceData::ClusterDataIndex( const AliHLTTPCCARow &row, int hitIndex ) const
{
  return row.fClusterDataIndex[hitIndex];
}

inline const AliHLTTPCCARow &AliHLTTPCCASliceData::Row( int rowIndex ) const
{
  return fRows[rowIndex];
}

inline uint_v AliHLTTPCCASliceData::CalculateHitWeight( uint_v numberOfHits, uint_v unique )
{
//  return ( numberOfHits << 8 ) | ( unique & 0xff ); // CHECKME we don't need this unique?
  return ( numberOfHits << 8 ) | ( unique & 0 );
}

inline uint_m AliHLTTPCCASliceData::TakeOwnHits( const AliHLTTPCCARow &row,
    const uint_v &hitIndex, const uint_m &mask, const uint_v &weights ) const
{
  const uint_v storedWeights( row.fHitWeights, hitIndex, mask );
  const uint_m own = storedWeights == weights && mask;
  const uint_v takenMarker = std::numeric_limits<uint_v>::max();
  takenMarker.scatter( row.fHitWeights, hitIndex, own );
  return own;
}

inline void AliHLTTPCCASliceData::MaximizeHitWeight( const AliHLTTPCCARow &row,
    const uint_v &hitIndex, const uint_v &weight )
{
  const int_m mask = validHitIndexes( hitIndex );
  VALGRIND_CHECK_VALUE_IS_DEFINED( weight );
#ifndef NVALGRIND
  for ( int i = 0; i < hitIndex.Size; ++i ) {
    if ( mask[i] ) {
      VALGRIND_CHECK_VALUE_IS_DEFINED( row.fHitWeights[hitIndex[i]] );
    }
  }
#endif
  // XXX critical section if the TrackletConstructor gets multi-threaded
  const uint_v oldWeight( row.fHitWeights, hitIndex, mask );
  debugF() << "scatter HitWeigths " << weight << " to " << hitIndex << ( weight > oldWeight && mask ) << " old: " << oldWeight << std::endl;
  weight.scatter( row.fHitWeights, hitIndex, weight > oldWeight && mask );
}

inline uint_v AliHLTTPCCASliceData::HitWeight( const AliHLTTPCCARow &row, const uint_v &hitIndex, const uint_m &mask ) const
{
#ifndef NVALGRIND
  for ( int i = 0; i < hitIndex.Size; ++i ) {
    if ( mask[i] ) {
      debugF() << i << ": " << hitIndex[i] << std::endl;
      VALGRIND_CHECK_VALUE_IS_DEFINED( row.fHitWeights[hitIndex[i]] );
    }
  }
#endif
  return uint_v( row.fHitWeights, hitIndex, mask );
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
