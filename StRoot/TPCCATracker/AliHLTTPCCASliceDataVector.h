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
    void SetHitLinkUpData( const AliHLTTPCCARow &row, int hitIndex, int value );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value );
    void SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value);
    void SetUnusedHitLinkUpData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowUp, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetUnusedHitLinkDownData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowDn, const uint_v &hitIndexes, const int_v &value, const int_m &mask );
    void SetUnusedHitLinkDataScalar( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowUp, const AliHLTTPCCARow &rowDn, unsigned int *hitIndexes );
    void SetHitLinkUpData  ( const AliHLTTPCCARow &row, const unsigned int hitIndex, const int value );

    void GetHitCoordinateVectors( const AliHLTTPCCARow &row, const vector<unsigned int> &indexes, float_v *Y, float_v *Z );

    /**
     * Reset all links to -1.
     */
    void ClearLinks();

    /**
     * Return the y and z coordinate(s) of the given hit(s).
     */
    float HitDataXS( const AliHLTTPCCARow &row, int hitIndex ) const;
    float HitPDataYS( const AliHLTTPCCARow &row, int hitIndex ) const;
    float HitPDataZS( const AliHLTTPCCARow &row, int hitIndex ) const;

    const int *HitDataIsUsed( const AliHLTTPCCARow &row ) const;
    float_v HitPDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    float_v HitPDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const;
    float_v HitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v HitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v UnusedHitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float_v UnusedHitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const;
    float UnusedHitPDataY( const AliHLTTPCCARow &row, const unsigned int hitIndex );
    float UnusedHitPDataZ( const AliHLTTPCCARow &row, const unsigned int hitIndex );
    void SetHitAsUsed( const AliHLTTPCCARow &row, const unsigned int hitIndex );

    void SetHitAsUsed( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );
    void SetHitAsUsedInTrackFit( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );
    void SetHitAsUsedInTrackExtend( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );

    void SetHitAsUsedInStartSegment( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask );

    void CleanUsedHits( int rowIndex, bool isFirst );
    void CleanUsedHitsType( int rowIndex, int type );
  
    /**
     * For a given bin index, content tells how many hits there are in the preceding bins. This maps
     * directly to the hit index in the given row.
     *
     * \param binIndexes in the range 0 to row.Grid.N + row.Grid.Ny + 3.
     */
    uint_v FirstHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const;
    const unsigned int *FirstHitInBin( const AliHLTTPCCARow &row ) const;
    unsigned int FirstHitInBin( const AliHLTTPCCARow &row, unsigned int binIndex ) const;
    unsigned int FirstUnusedHitInBin( const AliHLTTPCCARow &row, unsigned int binIndex ) const;
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

    void createGrid( AliHLTTPCCARow *row, const AliHLTTPCCAClusterData &data, const int clusterDataOffset, const int iRow );

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
  int_v r;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    r[i] = row.fLinkUpData[(unsigned int)hitIndexes[i]];
  }
  return r;
}

inline int_v AliHLTTPCCASliceData::HitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes ) const
{
  int_v r;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    r[i] = row.fLinkDownData[(unsigned int)hitIndexes[i]];
  }
  return r;
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData( const AliHLTTPCCARow &row, int hitIndex, int value )
{
  row.fLinkUpData[hitIndex] = value;
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value )
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( int_v::EntryType ) % VectorAlignment == 0 );
//  value.store( &row.fLinkUpData[hitIndex] );
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const int_i &hitIndex, const int_v &value )
{
  //Matthias 01.24.13  assert( hitIndex * sizeof( int_v::EntryType ) % VectorAlignment == 0 );
//  value.store( &row.fLinkDownData[hitIndex] );
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{ 
#ifdef VC_GATHER_SCATTER
   value.scatter( row.fLinkUpData, hitIndexes, mask );
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    row.fLinkUpData[(unsigned int)hitIndexes[i]] = value[i];
  }
#endif
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{ 
#ifdef VC_GATHER_SCATTER
  value.scatter( row.fLinkDownData, hitIndexes, mask );
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    row.fLinkDownData[(unsigned int)hitIndexes[i]] = value[i];
  }
#endif
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value)
{
#ifdef VC_GATHER_SCATTER
  value.scatter( row.fLinkUpData, hitIndexes);
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    row.fLinkUpData[(unsigned int)hitIndexes[i]] = value[i];
  }
#endif
}

inline void AliHLTTPCCASliceData::SetHitLinkUpData  ( const AliHLTTPCCARow &row, const unsigned int hitIndex, const int value )
{
//  row.fLinkUpData[row.HitIndex()[hitIndex]] = row.HitIndex()[value];
  row.fLinkUpData[hitIndex] = value;
}

inline void AliHLTTPCCASliceData::SetHitLinkDownData( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_v &value)
{
#ifdef VC_GATHER_SCATTER
  value.scatter( row.fLinkDownData, hitIndexes);
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    row.fLinkDownData[(unsigned int)hitIndexes[i]] = value[i];
  }
#endif
}

inline void AliHLTTPCCASliceData::SetUnusedHitLinkUpData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowUp, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{
#ifndef VC_GATHER_SCATTER
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    if( value[i] == -1 ) {
	row.fLinkUpData[(unsigned int)row.fHitIndex[(unsigned int)hitIndexes[i]]] = -1;
	continue;
    }
    row.fLinkUpData[(unsigned int)row.fHitIndex[(unsigned int)hitIndexes[i]]] = rowUp.fHitIndex[(unsigned int)value[i]];
  }
#else
  const uint_v ind( row.fHitIndex, hitIndexes, mask );
  uint_v val(rowUp.fHitIndex, static_cast<uint_v>(value), mask && value != -1);
  int_v val2(val);
  val2(value == -1) = value;
  val2.scatter( row.fLinkUpData, ind, mask );
#endif
}

inline void AliHLTTPCCASliceData::SetUnusedHitLinkDownData( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowDn, const uint_v &hitIndexes, const int_v &value, const int_m &mask )
{
#ifndef VC_GATHER_SCATTER
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    if( value[i] == -1 ) {
	row.fLinkDownData[(unsigned int)row.fHitIndex[(unsigned int)hitIndexes[i]]] = -1;
	continue;
    }
    row.fLinkDownData[(unsigned int)row.fHitIndex[(unsigned int)hitIndexes[i]]] = rowDn.fHitIndex[(unsigned int)value[i]];
  }
#else
  const uint_v ind( row.fHitIndex, hitIndexes, mask );
  uint_v val(rowDn.fHitIndex, static_cast<uint_v>(value), mask && value != -1);
  int_v val2(val);
  val2(value == -1) = value;
  val2.scatter( row.fLinkDownData, ind, mask );
#endif
}

inline void AliHLTTPCCASliceData::SetUnusedHitLinkDataScalar( const AliHLTTPCCARow &row, const AliHLTTPCCARow &rowUp, const AliHLTTPCCARow &rowDn, unsigned int *hitIndexes )
{
  row.fLinkUpData[(unsigned int)row.fHitIndex[hitIndexes[1]]] = rowUp.fHitIndex[hitIndexes[2]];
  row.fLinkDownData[(unsigned int)row.fHitIndex[hitIndexes[1]]] = rowDn.fHitIndex[hitIndexes[0]];
}

inline void AliHLTTPCCASliceData::GetHitCoordinateVectors( const AliHLTTPCCARow &row, const vector<unsigned int> &indexes, float_v *Y, float_v *Z )
{
  for( unsigned int iHv = 0; iHv < std::min(indexes.size(), float_v::Size); iHv++ ) {	//TODO indexes.size() - slow!
    (*Y)[iHv] = row.fUnusedHitPDataY[indexes[iHv]];
    (*Z)[iHv] = row.fUnusedHitPDataZ[indexes[iHv]];
  }
  (*Y) *= float_v(1e-2);
  (*Z) *= float_v(1e-2);
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
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    row.fHitDataIsUsed[(unsigned int)hitIndexes[i]] = 1;
  }
}

inline void AliHLTTPCCASliceData::SetHitAsUsed( const AliHLTTPCCARow &row, const unsigned int hitIndex )
{
  row.fHitDataIsUsed[hitIndex] = 1;
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInStartSegment( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask )
{
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    row.fHitDataIsUsed[(unsigned int)hitIndexes[i]] = 3;
  }
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInTrackFit( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask )
{
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    row.fHitDataIsUsed[(unsigned int)hitIndexes[i]] = 2;
  }
}

inline void AliHLTTPCCASliceData::SetHitAsUsedInTrackExtend( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const int_m &mask )
{
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    row.fHitDataIsUsed[(unsigned int)hitIndexes[i]] = 3;
  }
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
#ifdef VC_GATHER_SCATTER
      hitIndexes.scatter( row.fHitIndex, hitIndexes, validHitsMask );
#else
      for( unsigned int j = 0; j < float_v::Size; j++ ) {
	if( !validHitsMask[j] ) continue;
	row.fHitIndex[(unsigned int)hitIndexes[j]] = hitIndexes[j];
      }
#endif
    }
    row.fNUnusedHits = numberOfHits;
  }
  else {
    std::vector<unsigned int> unusedHitIndex(numberOfHits+1,numberOfHits);
    unsigned int iUH = 0;
    for ( unsigned int i = 0; i < numberOfHits; i += uint_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      int_v hitDataTemp;
      for( unsigned int ii = 0; ii < float_v::Size; ii++ ) {
	hitDataTemp[ii] = HitDataIsUsed( row )[(unsigned int)hitIndexes[ii]];
      }
      const int_m validHitsMask = (hitIndexes < numberOfHits) && ( hitDataTemp == int_v( Vc::Zero ) );
      for(unsigned int iV=0; iV<int_v::Size; iV++)
      {
        if(!validHitsMask[iV]) continue;
        row.fUnusedHitPDataY[iUH] = row.fHitPDataY[(unsigned int)hitIndexes[iV]];
        row.fUnusedHitPDataZ[iUH] = row.fHitPDataZ[(unsigned int)hitIndexes[iV]];
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

inline void AliHLTTPCCASliceData::CleanUsedHitsType( int rowIndex, int type )
{
  AliHLTTPCCARow &row = fRows[rowIndex];
  const unsigned int numberOfHits = row.NHits();
  if( numberOfHits < 1 ) return;

  const unsigned int NFirstHitInBin = row.Grid().N() + row.Grid().Ny() + 3;
    std::vector<unsigned int> unusedHitIndex(numberOfHits+1,numberOfHits);
    unsigned int iUH = 0;
    for ( unsigned int i = 0; i < numberOfHits; i += uint_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      int_v hitDataTemp;
      for( unsigned int ii = 0; ii < float_v::Size; ii++ ) {
	hitDataTemp[ii] = HitDataIsUsed( row )[(unsigned int)hitIndexes[ii]];
      }
      const int_m validHitsMask = (hitIndexes < numberOfHits) && ( hitDataTemp != int_v( type ) );
      for(unsigned int iV=0; iV<int_v::Size; iV++)
      {
        if(!validHitsMask[iV]) continue;
        row.fUnusedHitPDataY[iUH] = row.fHitPDataY[(unsigned int)hitIndexes[iV]];
        row.fUnusedHitPDataZ[iUH] = row.fHitPDataZ[(unsigned int)hitIndexes[iV]];
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

inline float_v AliHLTTPCCASliceData::HitPDataY( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
{
  float_v r;
  for(unsigned  int i = 0; i < float_v::Size; i++){
    r[i] = row.fHitPDataY[hitIndex + i];
  }
  r *= float_v(1e-2);
  return r;
}

inline float_v AliHLTTPCCASliceData::HitPDataZ( const AliHLTTPCCARow &row, const uint_i &hitIndex ) const
{
  float_v r;
  for(unsigned  int i = 0; i < float_v::Size; i++){ 
    r[i] = row.fHitPDataZ[hitIndex + i];
  }
  r *= float_v(1e-2);
  return r;
}

inline float_v AliHLTTPCCASliceData::HitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  float_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
    r[i] = row.fHitPDataY[(unsigned int)hitIndexes[i]];
  }
  r *= float_v(1e-2);
  return r;
}

inline float_v AliHLTTPCCASliceData::HitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  float_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
    r[i] = row.fHitPDataZ[(unsigned int)hitIndexes[i]];
  }
  r *= float_v(1e-2);
  return r;
}

inline float_v AliHLTTPCCASliceData::UnusedHitPDataY( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  float_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
    r[i] = row.fUnusedHitPDataY[(unsigned int)hitIndexes[i]];
  }
  r *= float_v(1e-2);
  return r;
}

inline float_v AliHLTTPCCASliceData::UnusedHitPDataZ( const AliHLTTPCCARow &row, const uint_v &hitIndexes, const float_m &mask ) const
{
  float_v r;
  for(unsigned int i=0; i<float_v::Size; i++)
  {
    if(!mask[i]) continue;
    r[i] = row.fUnusedHitPDataZ[(unsigned int)hitIndexes[i]];
  }
  r *= float_v(1e-2);
  return r;
}

inline float AliHLTTPCCASliceData::UnusedHitPDataY( const AliHLTTPCCARow &row, const unsigned int hitIndex )
{
  return row.fUnusedHitPDataY[hitIndex] * 1e-2;
}

inline float AliHLTTPCCASliceData::UnusedHitPDataZ( const AliHLTTPCCARow &row, const unsigned int hitIndex )
{
  return row.fUnusedHitPDataZ[hitIndex] * 1e-2;
}

inline uint_v AliHLTTPCCASliceData::FirstHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const
{
  uint_v tmp;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    tmp[i] = row.fFirstHitInBin[(unsigned int)binIndexes[i]];
  }
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

inline unsigned int AliHLTTPCCASliceData::FirstUnusedHitInBin( const AliHLTTPCCARow &row, unsigned int binIndex ) const
{
  return row.fFirstHitInBin[binIndex];
}

inline uint_v AliHLTTPCCASliceData::FirstUnusedHitInBin( const AliHLTTPCCARow &row, uint_v binIndexes ) const
{
  uint_v tmp;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    tmp[i] = row.fFirstUnusedHitInBin[(unsigned int)binIndexes[i]];
  }
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

inline uint_m AliHLTTPCCASliceData::TakeOwnHits( const AliHLTTPCCARow &row,
    const uint_v &hitIndex, const uint_m &mask, const uint_v &weights ) const
{
  int_v storedWeights;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    storedWeights[i] = row.fHitWeights[(unsigned int)hitIndex[i]];
  }
  const uint_m own = storedWeights == weights && mask;
  const uint_v takenMarker = std::numeric_limits<uint_v>::max();
#ifdef VC_GATHER_SCATTER
  takenMarker.scatter( row.fHitWeights, hitIndex, own );
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !own[i] ) continue;
    row.fHitWeights[(unsigned int)hitIndex[i]] = takenMarker[i];
  }
#endif
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
  int_v oldWeight;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    oldWeight[i] = row.fHitWeights[(unsigned int)hitIndex[i]];
  }
  debugF() << "scatter HitWeigths " << weight << " to " << hitIndex << ( weight > oldWeight && mask ) << " old: " << oldWeight << std::endl;
  int_m maskTemp = weight > oldWeight && mask;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !maskTemp[i] ) continue;
    row.fHitWeights[(unsigned int)hitIndex[i]] = weight[i];
  }
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
//  return uint_v( row.fHitWeights, hitIndex, mask );
  uint_v r;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    if( !mask[i] ) continue;
    r[i] = row.fHitWeights[(unsigned int)hitIndex[i]];
  }
  return 0;
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

inline void AliHLTTPCCASliceData::createGrid( AliHLTTPCCARow *row, const AliHLTTPCCAClusterData &data, const int clusterDataOffset, const int iRow )
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
