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


#ifndef ALIHLTTPCCAMERGEROUTPUT_H
#define ALIHLTTPCCAMERGEROUTPUT_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAMergedTrack.h"
#include "AliHLTTPCCADataCompressor.h"

/**
 * @class AliHLTTPCCAMergerOutput
 *
 * AliHLTTPCCAMergerOutput class is used to store the output of AliHLTTPCCATracker{Component}
 * and transport the output to AliHLTTPCCAMerger{Component}
 *
 * The class contains all the necessary information about TPC tracks, reconstructed in one slice.
 * This includes the reconstructed track parameters and some compressed information
 * about the assigned clusters: clusterId, position and amplitude.
 *
 */
class AliHLTTPCCAMergerOutput
{
  public:

    AliHLTTPCCAMergerOutput()
        : fNTracks( 0 ), fNTrackClusters( 0 ), fTracks( 0 ), fClusterIDsrc( 0 ), fClusterPackedAmp( 0 ) {}

    AliHLTTPCCAMergerOutput( const AliHLTTPCCAMergerOutput & )
        : fNTracks( 0 ), fNTrackClusters( 0 ), fTracks( 0 ), fClusterIDsrc( 0 ), fClusterPackedAmp( 0 ) {}

    const AliHLTTPCCAMergerOutput& operator=( const AliHLTTPCCAMergerOutput &/*v*/ ) const {
      return *this;
    }

    ~AliHLTTPCCAMergerOutput() {}


    int NTracks()                    const { return fNTracks;              }
    int NTrackClusters()             const { return fNTrackClusters;       }

    const AliHLTTPCCAMergedTrack &Track( int i ) const { return fTracks[i]; }
    AliHLTTPCCAMergedTrack &Track( int i ) { return fTracks[i]; }
    const DataCompressor::SliceRowCluster &ClusterIDsrc     ( int i )  const { return fClusterIDsrc[i]; }
    UChar_t  ClusterPackedAmp( int i )  const { return fClusterPackedAmp[i]; }

    static int EstimateSize( int nOfTracks, int nOfTrackClusters );
    void SetPointers();

    void SetNTracks       ( int v )  { fNTracks = v;        }
    void SetNTrackClusters( int v )  { fNTrackClusters = v; }

    void SetTrack( int i, const AliHLTTPCCAMergedTrack &v ) {  fTracks[i] = v; }
    void SetClusterIDsrc( int i, const DataCompressor::SliceRowCluster &v ) {  fClusterIDsrc[i] = v; }
    void SetClusterPackedAmp( int i, UChar_t v ) {  fClusterPackedAmp[i] = v; }

    // ---
    void SetMerged( int itr ) { fTracks[itr].SetMerged(); }
    bool IsMerged( int itr ) { return fTracks[itr].IsMerged(); }
    // ---

  private:

    int fNTracks;                 // number of reconstructed tracks
    int fNTrackClusters;          // total number of track clusters
    AliHLTTPCCAMergedTrack *fTracks; // pointer to reconstructed tracks
    DataCompressor::SliceRowCluster *fClusterIDsrc;         // pointer to cluster IDs ( packed IRow and ICluster)
    UChar_t  *fClusterPackedAmp;    // pointer to packed cluster amplitudes

};



inline int AliHLTTPCCAMergerOutput::EstimateSize( int nOfTracks, int nOfTrackClusters )
{
  // calculate the amount of memory [bytes] needed for the event

  const int kClusterDataSize = sizeof( unsigned int ) + sizeof( UChar_t );

  return sizeof( AliHLTTPCCAMergerOutput ) + sizeof( AliHLTTPCCAMergedTrack )*nOfTracks + kClusterDataSize*nOfTrackClusters;
}


inline void AliHLTTPCCAMergerOutput::SetPointers()
{
  // set all pointers

  fTracks            = ( AliHLTTPCCAMergedTrack* )( ( &fClusterPackedAmp ) + 1 );
  fClusterIDsrc      = ( DataCompressor::SliceRowCluster* )  ( fTracks            + fNTracks );
  fClusterPackedAmp  = ( UChar_t* ) ( fClusterIDsrc + fNTrackClusters );
}

#endif
