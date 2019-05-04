// @(#) $Id: AliHLTTPCCASliceOutput.cxx,v 1.3 2012/08/13 19:35:05 fisyak Exp $
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

#include "AliHLTTPCCASliceOutput.h"
#include "MemoryAssignmentHelpers.h"

int AliHLTTPCCASliceOutput::EstimateSize( int nOfTracks, int nOfTrackClusters )
{
  // // calculate the amount of memory [bytes] needed for the event

  const int kClusterDataSize =
    sizeof( DataCompressor::RowCluster )
    + sizeof( unsigned short )
    + sizeof( float2 )
    + sizeof( float )
    + sizeof( UChar_t );

  const int aligmentMaxSize = 
    kClusterDataSize; // aligment during AssignMemory
#ifndef TETA
    + sizeof( AliHLTTPCCASliceTrack )
#else
    + sizeof( AliHLTTPCCASliceTrackVector )
#endif
    ;
  return
    kClusterDataSize * nOfTrackClusters
    + aligmentMaxSize
#ifndef TETA
    + sizeof( AliHLTTPCCASliceTrack ) * nOfTracks
#else
    + sizeof( AliHLTTPCCASliceTrackVector ) * (int)( 1 + nOfTracks/float_v::Size )
#endif
    ;

  // return
  //   RequiredMemory(fTracks, fNTracks) +
  // RequiredMemory( fClusterUnpackedYZ, fNTrackClusters ) +
  // RequiredMemory( fClusterUnpackedX,  fNTrackClusters ) +
  // RequiredMemory( fClusterIDrc,       fNTrackClusters ) +
  // RequiredMemory( fClusterPackedYZ,   fNTrackClusters ) +
  // RequiredMemory( fClusterPackedAmp,  fNTrackClusters );
}


void AliHLTTPCCASliceOutput::SetPointers()
{
  // set all pointers

  char *mem = &fMemory[0];
#ifndef TETA
  AssignMemory( fTracks,            mem, fNTracks );
#else
  AssignMemoryV( fTracksV,           mem, (int)(1 + fNTracks/float_v::Size) );
#endif
  AssignMemory( fClusterUnpackedYZ, mem, fNTrackClusters );
  AssignMemory( fClusterUnpackedX,  mem, fNTrackClusters );
  AssignMemory( fClusterIDrc,       mem, fNTrackClusters );
  AssignMemory( fClusterPackedYZ,   mem, fNTrackClusters );
  AssignMemory( fClusterPackedAmp,  mem, fNTrackClusters );
}

