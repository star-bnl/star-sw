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
//#ifndef TETA
//    + sizeof( AliHLTTPCCASliceTrack )
//#else
//    + sizeof( AliHLTTPCCASliceTrackVector )
//#endif
//    ;
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

