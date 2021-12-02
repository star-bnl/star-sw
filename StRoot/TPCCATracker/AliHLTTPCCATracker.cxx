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

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCATrack.h"
#include "AliHLTTPCCATrackletVector.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCAHit.h"
#include "Reconstructor.h"
#include "MemoryAssignmentHelpers.h"

#include "Stopwatch.h"
#include "AliHLTTPCCASliceOutput.h"
#include "AliHLTTPCCADataCompressor.h"
#include "AliHLTTPCCAClusterData.h"

#include "AliHLTTPCCATrackParam.h"

#include <iostream>

#ifdef HLTCA_INTERNAL_PERFORMANCE
#include "AliHLTTPCCAPerformance.h"
#endif

using std::endl;

AliHLTTPCCATracker::AliHLTTPCCATracker()
    :
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    fNOutTracks1( 0 ),
    fOutTracks1( 0 ),
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
    fParam(),
    fClusterData( 0 ),
    fHitMemory( 0 ),
    fHitMemorySize( 0 ),
    fTrackMemory( 0 ),
    fTrackMemorySize( 0 ),
    fTrackletStartHits( 0 ),
    fNTracklets( 0 ),
    fNTrackHits( 0 ),
    fOutput( 0 )
{
  // constructor
}

AliHLTTPCCATracker::~AliHLTTPCCATracker()
{
  // destructor
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    if (fOutTracks1) delete[] fOutTracks1;
#endif
  if (fHitMemory) delete[] fHitMemory;
  if (fTrackMemory) delete[] fTrackMemory;
}



// ----------------------------------------------------------------------------------
void AliHLTTPCCATracker::Initialize( const AliHLTTPCCAParam &param )
{
  // initialisation
  fParam = param;
  fParam.Update();
  fData.InitializeRows( fParam );

  StartEvent();
}

void AliHLTTPCCATracker::StartEvent()
{
  // start new event and fresh the memory

  SetupCommonMemory();
  fNTrackHits = 0;
}

void  AliHLTTPCCATracker::SetupCommonMemory()
{
//   if (fHitMemory) delete[] fHitMemory;
//   fHitMemory = 0;
  if (fTrackMemory) delete[] fTrackMemory;
  fTrackMemory = 0;

  fData.Clear();
  fNTracklets = 0;
}

void AliHLTTPCCATracker::RecalculateHitsSize( int MaxNHits )
{
//   fHitMemorySize = 0;
//   fHitMemorySize += RequiredMemory( fTrackletStartHits, MaxNHits );
}

void  AliHLTTPCCATracker::SetPointersHits( int MaxNHits )
{
//   assert( fHitMemory );
//   assert( fHitMemorySize > 0 );
  // set all pointers to the event memory
//   char *mem = fHitMemory;
  // extra arrays for tpc clusters
  fTrackletStartHits.resize(MaxNHits);
  //AssignMemory( fTrackletStartHits, mem, MaxNHits );
  // arrays for track hits
//   assert( fHitMemorySize >= mem - fHitMemory );
}

void AliHLTTPCCATracker::RecalculateTrackMemorySize( int MaxNTracks, int MaxNHits )
{
  debugWO() << "RecalculateTrackMemorySize( " << MaxNTracks << ", " << MaxNHits << ")" << endl;
  fTrackMemorySize = 0;
  fTrackMemorySize += sizeof( void * ); // alignment
  fTrackMemorySize += sizeof(AliHLTTPCCASliceOutput);
  fTrackMemorySize += AliHLTTPCCASliceOutput::EstimateSize( MaxNTracks, MaxNHits );
}

void  AliHLTTPCCATracker::SetPointersTracks( int MaxNTracks, int MaxNHits )
{
  debugWO() << "SetPointersTracks( " << MaxNTracks << ", " << MaxNHits << ")" << endl;
  assert( fTrackMemory );
  assert( fTrackMemorySize > 0 );

  // set all pointers to the tracks memory

  char *mem = fTrackMemory;

  // memory for output

  AlignTo<sizeof( void * )>( mem );
  fOutput = new( mem ) AliHLTTPCCASliceOutput;
  mem += sizeof(AliHLTTPCCASliceOutput);
  mem += AliHLTTPCCASliceOutput::EstimateSize( MaxNTracks, MaxNHits );

  // memory for output tracks
  assert( fTrackMemorySize >= mem - fTrackMemory );
}


void AliHLTTPCCATracker::ReadEvent( AliHLTTPCCAClusterData *clusterData )
{
  fClusterData = clusterData;
  StartEvent();
  //* Convert input hits, create grids, etc.
  fData.InitFromClusterData( *clusterData );
  {
    RecalculateHitsSize( fData.NumberOfHits() ); // to calculate the size
//     fHitMemory = new char[fHitMemorySize + 1600];
    SetPointersHits( fData.NumberOfHits() ); // set pointers for hits
    fNTracklets = 0;
  }
}

void AliHLTTPCCATracker::Reconstruct()
{
#ifdef USE_TBB
  tbb::task::spawn_root_and_wait( *new( tbb::task::allocate_root() ) Reconstructor( this ) );
#else //USE_TBB
  Reconstructor R(this);
  R.execute();
#endif //USE_TBB
}

void AliHLTTPCCATracker::WriteOutput()
{
  // write output
#ifdef USE_TIMERS
  Stopwatch timer;
  timer.Start();
#endif // USE_TIMERS

  debugWO() << "numberOfTracks = " << fNumberOfTracks << std::endl;
  fOutput->SetNTracks( fNumberOfTracks );
  fOutput->SetNTrackClusters( fNTrackHits );
  fOutput->SetPointers();

  debugWO() << "WriteOutput| "
    << fNumberOfTracks << " tracks found, "
    << fTrackletVectors.Size() << " TrackletVectors, "
    << fNTrackHits << " track hits "
    << std::endl;

  int hitStoreIndex = 0;
  int nStoredHits = 0;
  int iTr = 0;
  const int tracksSize = fTracks.size();
#ifdef TETA
  float *sFirstClusterRef = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *sNClusters = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *sInnerRow = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  float *sOuterRow = (float*) _mm_malloc(sizeof(float_v), float_v::Size*4);
  const AliHLTTPCCATrackParam *sStartPoint[float_v::Size] = {0};
  int nTrackV(0), iTrV(0);
  fOutput->SetNTracksV( 0 );
  TrSort* tr_sort_helper = new TrSort[fNumberOfTracks];
  for ( int trackIndex = 0; trackIndex < tracksSize; ++trackIndex ) {
    if( trackIndex >= fNumberOfTracks ) continue;
    const Track &track = *fTracks[trackIndex];
    tr_sort_helper[trackIndex].nHits = track.NumberOfHits();
    tr_sort_helper[trackIndex].trId = trackIndex;
  }
  std::sort(&(tr_sort_helper[0]), &(tr_sort_helper[fNumberOfTracks]), TrSort::trComp);
#endif
  for ( int trackIndex = 0; trackIndex < tracksSize; ++trackIndex ) {
    // if (!fTracks[trackIndex]) continue;
#ifndef TETA
    const Track &track = *fTracks[trackIndex];
    const int numberOfHits = track.NumberOfHits();

    {
      AliHLTTPCCASliceTrack out;
      hitStoreIndex = nStoredHits;
      out.SetFirstClusterRef( nStoredHits );
//      nStoredHits += numberOfHits;
      out.SetNClusters( numberOfHits );
      out.SetParam( track.Param() );
      fOutput->SetTrack( iTr, out );
    }
#else
    const Track &track = *fTracks[tr_sort_helper[trackIndex].trId];
    const int numberOfHits = track.NumberOfHits();
    sFirstClusterRef[nTrackV] = nStoredHits;
    sNClusters[nTrackV] = numberOfHits;
    sInnerRow[nTrackV] = track.HitId( 0 ).RowIndex();
    sOuterRow[nTrackV] = track.HitId( numberOfHits - 1 ).RowIndex();
    sStartPoint[nTrackV] = &track.Param();
    nTrackV++;
    if( nTrackV == float_v::Size || trackIndex >= tracksSize - 1 ) {
      float_v &tFirstClusterRef = reinterpret_cast<float_v&>(sFirstClusterRef[0]);
      int_v vFirstClusterRef(tFirstClusterRef);
      float_v &tNClusters = reinterpret_cast<float_v&>(sNClusters[0]);
      int_v vNClusters(tNClusters);
      float_v &tInnerRow = reinterpret_cast<float_v&>(sInnerRow[0]);
      int_v vInnerRow(tInnerRow);
      float_v &tOuterRow = reinterpret_cast<float_v&>(sOuterRow[0]);
      int_v vOuterRow(tOuterRow);
      AliHLTTPCCATrackParamVector vStartPoint;
      ConvertPTrackParamToVector(sStartPoint,vStartPoint,nTrackV);
      int_m vActive( int_v( Vc::IndexesFromZero ) < nTrackV );
      AliHLTTPCCASliceTrackVector vOut( vFirstClusterRef, vNClusters, vInnerRow, vOuterRow, int_v(0), vActive, vStartPoint );
      fOutput->SetTrackV( iTrV, vOut );
      iTrV++;
      nTrackV = 0;
      fOutput->AddNTracksV();
    }
#endif
    ++iTr;
    nStoredHits += numberOfHits;

    for ( int hitIdIndex = 0; hitIdIndex < numberOfHits; ++hitIdIndex ) {
      const HitId &hitId = track.HitId( hitIdIndex );
      const int rowIndex = hitId.RowIndex();
      const unsigned int hitIndex = hitId.HitIndex();
      const AliHLTTPCCARow &row = fData.Row( rowIndex );

      const int inpIdOffset = fClusterData->RowOffset( rowIndex );
      const int inpIdtot = fData.ClusterDataIndex( row, hitIndex );
      const int inpId = inpIdtot - inpIdOffset;
#if 0
      VALGRIND_CHECK_VALUE_IS_DEFINED( rowIndex );
      VALGRIND_CHECK_VALUE_IS_DEFINED( hitIndex );
      VALGRIND_CHECK_VALUE_IS_DEFINED( inpIdOffset );
      VALGRIND_CHECK_VALUE_IS_DEFINED( inpIdtot );
      VALGRIND_CHECK_VALUE_IS_DEFINED( inpId );
#endif

      const float origX = fClusterData->X( inpIdtot );
      const float origY = fClusterData->Y( inpIdtot );
      const float origZ = fClusterData->Z( inpIdtot );

      const DataCompressor::RowCluster rc( rowIndex, inpId );
      unsigned int hPackedYZ = 0;
      UChar_t hPackedAmp = 0;
      float2 hUnpackedYZ;
      hUnpackedYZ.x = origY;
      hUnpackedYZ.y = origZ;
      float hUnpackedX = origX;

      fOutput->SetClusterIDrc( hitStoreIndex, rc  );
      fOutput->SetClusterPackedYZ( hitStoreIndex, hPackedYZ );
      fOutput->SetClusterPackedAmp( hitStoreIndex, hPackedAmp );
      fOutput->SetClusterUnpackedYZ( hitStoreIndex, hUnpackedYZ );
      fOutput->SetClusterUnpackedX( hitStoreIndex, hUnpackedX );
      ++hitStoreIndex;
    }
#ifndef TETA
    if (fTracks[trackIndex]) delete fTracks[trackIndex];
#endif
  }
#ifdef TETA
  _mm_free(sFirstClusterRef);
  _mm_free(sNClusters);
  _mm_free(sInnerRow);
  _mm_free(sOuterRow);
#else
  fOutput->SortTracks();
#endif

#ifdef USE_TIMERS
  timer.Stop();
  fTimers[5] += timer.RealTime();
#endif // USE_TIMERS

}

void AliHLTTPCCATracker::GetErrors2( int iRow, const AliHLTTPCCATrackParam &t, float *Err2Y, float *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  fParam.GetClusterErrors2( iRow, t, *Err2Y, *Err2Z );
}

void AliHLTTPCCATracker::WriteTracks( std::ostream &out )
{
  //* Write tracks to file

  out << fTimers[0] << std::endl;
#if 0
  out << fNOutTrackHits << std::endl;
  for ( int hitIndex = 0; hitIndex < fNOutTrackHits; ++hitIndex ) {
    out << fOutTrackHits[hitIndex] << " ";
  }
  out << std::endl;

  out << fNOutTracks << std::endl;

  for ( int itr = 0; itr < fNOutTracks; itr++ ) {
    out << fOutTracks[itr];
  }
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
}

void AliHLTTPCCATracker::ReadTracks( std::istream &in )
{
  //* Read tracks  from file
  in >> fTimers[0];
#if 0
  in >> fNOutTrackHits;

  for ( int hitIndex = 0; hitIndex < fNOutTrackHits; ++hitIndex ) {
    in >> fOutTrackHits[hitIndex];
  }
  in >> fNOutTracks;

  for ( int itr = 0; itr < fNOutTracks; itr++ ) {
    in >> fOutTracks[itr];
  }
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
}

#include "BinaryStoreHelper.h"

void AliHLTTPCCATracker::StoreToFile( FILE *f ) const
{
  fParam.StoreToFile( f );
  fData.StoreToFile( f );

  BinaryStoreWrite( fTimers, 10, f );

  BinaryStoreWrite( fNTracklets, f );
#if 0
  BinaryStoreWrite( fNOutTracks, f );
  //BinaryStoreWrite( fOutTracks, startPointer, f );
  BinaryStoreWrite( fNOutTrackHits, f );
  //BinaryStoreWrite( fOutTrackHits, startPointer, f );
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
}

void AliHLTTPCCATracker::RestoreFromFile( FILE *f )
{
  fParam.RestoreFromFile( f );
  fData.RestoreFromFile( f );

  BinaryStoreRead( fTimers, 10, f );

  Byte_t alignment;
  BinaryStoreRead( alignment, f );

  BinaryStoreRead( fNTracklets, f );
#if 0
  BinaryStoreRead( fNOutTracks   , f );
  //BinaryStoreRead( fOutTracks    , startPointer, f );
  BinaryStoreRead( fNOutTrackHits, f );
  //BinaryStoreRead( fOutTrackHits , startPointer, f );
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
}
