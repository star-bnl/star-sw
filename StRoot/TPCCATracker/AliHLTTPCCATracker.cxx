// @(#) $Id: AliHLTTPCCATracker.cxx,v 1.3 2016/07/15 14:43:33 fisyak Exp $
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
    fData(),
    fHitMemory( 0 ),
    fHitMemorySize( 0 ),
    fTrackMemory( 0 ),
    fTrackMemorySize( 0 ),
    fTrackletStartHits( 0 ),
    fNTracklets( 0 ),
    fTrackletVectors(),
    fNumberOfTracks(0),
    fTracks(0),
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
//   std::cout << " SetupCommonMemory0 " << std::endl; //iklm debug
  if (fHitMemory) delete[] fHitMemory;
//   std::cout << " SetupCommonMemory1 " << std::endl; //iklm debug
  fHitMemory = 0;
  if (fTrackMemory) delete[] fTrackMemory;
//   std::cout << " SetupCommonMemory2 " << std::endl; //iklm debug
  fTrackMemory = 0;

  fData.Clear();
  fNTracklets = 0;
}

void AliHLTTPCCATracker::RecalculateHitsSize( int MaxNHits )
{
  fHitMemorySize = 0;
  fHitMemorySize += RequiredMemory( fTrackletStartHits, MaxNHits );
}

void  AliHLTTPCCATracker::SetPointersHits( int MaxNHits )
{
  assert( fHitMemory );
  assert( fHitMemorySize > 0 );

  // set all pointers to the event memory

  char *mem = fHitMemory;

  // extra arrays for tpc clusters

  AssignMemory( fTrackletStartHits, mem, MaxNHits );

  // arrays for track hits
  assert( fHitMemorySize >= mem - fHitMemory );
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
//   std::cout << " cat0 " << std::endl; //iklm debug
  StartEvent();
//   std::cout << " cat1 " << std::endl; //iklm debug
  //* Convert input hits, create grids, etc.
  fData.InitFromClusterData( *clusterData );
//   std::cout << " cat2 " << std::endl; //iklm debug
  {
    RecalculateHitsSize( fData.NumberOfHits() ); // to calculate the size
    fHitMemory = new char[fHitMemorySize + 1600];
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
  
  //cout<<"output: nTracks = "<< fTracks.size() <<", nHitsTotal="<<fData.NumberOfHits()<<std::endl;

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
  for ( int trackIndex = 0; trackIndex < tracksSize; ++trackIndex ) {
    // if (!fTracks[trackIndex]) continue;
    const Track &track = *fTracks[trackIndex];
    const short numberOfHits = track.NumberOfHits();
    if ( numberOfHits <= 0 ) {
      // might happen. See TrackletSelector.
      if (fTracks[trackIndex]) delete fTracks[trackIndex];
      continue;
    }

    {
      AliHLTTPCCASliceTrack out;
      hitStoreIndex = nStoredHits;
      out.SetFirstClusterRef( nStoredHits );
      nStoredHits += numberOfHits;
      out.SetNClusters( numberOfHits );
      out.SetParam( track.Param() );
      fOutput->SetTrack( iTr, out );
    }
    ++iTr;

    for ( int hitIdIndex = 0; hitIdIndex < numberOfHits; ++hitIdIndex ) {
      const HitId &hitId = track.HitId( hitIdIndex );
      const short rowIndex = hitId.RowIndex();
      const unsigned short hitIndex = hitId.HitIndex();
      const AliHLTTPCCARow &row = fData.Row( rowIndex );

      const int inpIdOffset = fClusterData->RowOffset( rowIndex );
      const int inpIdtot = fData.ClusterDataIndex( row, hitIndex );
      const int inpId = inpIdtot - inpIdOffset;

      const float origX = fClusterData->X( inpIdtot );
      const float origY = fClusterData->Y( inpIdtot );
      const float origZ = fClusterData->Z( inpIdtot );

      const DataCompressor::RowCluster rc( rowIndex, inpId );
      unsigned short hPackedYZ = 0;
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
    if (fTracks[trackIndex]) delete fTracks[trackIndex];
  }

  fOutput->SortTracks();
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

  //BinaryStoreWrite( fLinkUpData  , startPointer, f );
  //BinaryStoreWrite( fLinkDownData, startPointer, f );
  //BinaryStoreWrite( fHitDataY    , startPointer, f );
  //BinaryStoreWrite( fHitDataZ    , startPointer, f );

  //BinaryStoreWrite( fHitWeights, startPointer, f );

  BinaryStoreWrite( fNTracklets, f );
  //BinaryStoreWrite( fTrackletStartHits, startPointer, f );
  // TODO BinaryStoreWrite( fTracklets, startPointer, f );

  //BinaryStoreWrite( fTracks, startPointer, f );
  //BinaryStoreWrite( fNTrackHits, startPointer, f );
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

  //BinaryStoreRead( fLinkUpData   , startPointer, f );
  //BinaryStoreRead( fLinkDownData , startPointer, f );
  //BinaryStoreRead( fHitDataY     , startPointer, f );
  //BinaryStoreRead( fHitDataZ     , startPointer, f );

  //BinaryStoreRead( fHitWeights   , startPointer, f );

  BinaryStoreRead( fNTracklets, f );
  //BinaryStoreRead( fTrackletStartHits, startPointer, f );
  // TODO BinaryStoreRead( fTracklets    , startPointer, f );

  //BinaryStoreRead( fTracks       , startPointer, f );
  //BinaryStoreRead( fNTrackHits   , startPointer, f );
#if 0
  BinaryStoreRead( fNOutTracks   , f );
  //BinaryStoreRead( fOutTracks    , startPointer, f );
  BinaryStoreRead( fNOutTrackHits, f );
  //BinaryStoreRead( fOutTrackHits , startPointer, f );
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
}
