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


#ifndef ALIHLTTPCCASLICEOUTPUT_H
#define ALIHLTTPCCASLICEOUTPUT_H

#include "AliHLTTPCCADef.h"

#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCADataCompressor.h"
#include "AliHLTTPCCASliceTrackVector.h"

/**
 * @class AliHLTTPCCASliceOutput
 *
 * AliHLTTPCCASliceOutput class is used to store the output of AliHLTTPCCATracker{Component}
 * and transport the output to AliHLTTPCCAGBMerger{Component}
 *
 * The class contains all the necessary information about TPC tracks, reconstructed in one slice.
 * This includes the reconstructed track parameters and some compressed information
 * about the assigned clusters: clusterId, position and amplitude.
 *
 */
class AliHLTTPCCASliceOutput
{
  public:

    inline AliHLTTPCCASliceOutput() {}
    int NTracks()                    const { return fNTracks;              }
    int NTrackClusters()             const { return fNTrackClusters;       }

    const AliHLTTPCCASliceTrack &Track( int i ) const { return fTracks[i]; }
    const DataCompressor::RowCluster &ClusterIDrc( int i )  const { return fClusterIDrc[i]; }
    unsigned short ClusterPackedYZ ( int i )  const { return fClusterPackedYZ[i]; }
    UChar_t  ClusterPackedAmp( int i )  const { return fClusterPackedAmp[i]; }
    float2   ClusterUnpackedYZ ( int i )  const { return fClusterUnpackedYZ[i]; }
    float    ClusterUnpackedX  ( int i )  const { return fClusterUnpackedX[i]; }

    static int EstimateSize( int nOfTracks, int nOfTrackClusters );
    void SetPointers();

    void SetNTracks       ( int v )  { fNTracks = v;        }
    void SetNTrackClusters( int v )  { fNTrackClusters = v; }

    void SetTrack( int i, const AliHLTTPCCASliceTrack &v ) {  fTracks[i] = v; }
    void SetClusterIDrc( int i, const DataCompressor::RowCluster &v ) {  fClusterIDrc[i] = v; }
    void SetClusterPackedYZ( int i, unsigned short v ) {  fClusterPackedYZ[i] = v; }
    void SetClusterPackedAmp( int i, UChar_t v ) {  fClusterPackedAmp[i] = v; }
    void SetClusterUnpackedYZ( int i, float2 v ) {  fClusterUnpackedYZ[i] = v; }
    void SetClusterUnpackedX( int i, float v ) {  fClusterUnpackedX[i] = v; }

    void SortTracks() {if(!fTracks) return; std::sort(fTracks,fTracks+fNTracks,CompareTrackLength);}

    static bool CompareTrackLength (const AliHLTTPCCASliceTrack &st1, const AliHLTTPCCASliceTrack &st2) {
      return (st1.NClusters() > st2.NClusters());
    }
#if 0
    void SetNTracksV       ( int v )  { fNTracksV = v;        }
    void AddNTracksV       ()  { fNTracksV++;        }
    int NTracksV()                    const { return fNTracksV;              }
    AliHLTTPCCASliceTrackVector &TrackV( int i ) const { return fTracksV[i]; }
    void SetTrackV( int i, const AliHLTTPCCASliceTrackVector &v ) {  fTracksV[i] = v; }
#endif

  private:

    AliHLTTPCCASliceOutput( const AliHLTTPCCASliceOutput& )
        : fNTracks( 0 ), fNTrackClusters( 0 ), fTracks( 0 ), fClusterIDrc( 0 ), fClusterPackedYZ( 0 ), fClusterUnpackedYZ( 0 ), fClusterUnpackedX( 0 ), fClusterPackedAmp( 0 ) {}

    const AliHLTTPCCASliceOutput& operator=( const AliHLTTPCCASliceOutput& ) const { return *this; }

    int fNTracks;                 // number of reconstructed tracks
    int fNTrackClusters;          // total number of track clusters
    AliHLTTPCCASliceTrack *fTracks; // pointer to reconstructed tracks
#if 0
    int fNTracksV;                 // number of reconstructed tracks
    AliHLTTPCCASliceTrackVector *fTracksV; // pointer to reconstructed tracks
#endif
    DataCompressor::RowCluster *fClusterIDrc;  // pointer to cluster IDs ( packed IRow and ICluster)
    unsigned short *fClusterPackedYZ;     // pointer to packed cluster YZ coordinates
    float2   *fClusterUnpackedYZ;   // pointer to cluster coordinates (temporary data, for debug proposes)
    float    *fClusterUnpackedX;   // pointer to cluster coordinates (temporary data, for debug proposes)
    UChar_t  *fClusterPackedAmp;    // pointer to packed cluster amplitudes
    char fMemory[1]; // the memory where the pointers above point into
};

#endif
