//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCASLICEOUTPUT_H
#define ALIHLTTPCCASLICEOUTPUT_H

#include "AliHLTTPCCADef.h"

#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCADataCompressor.h"

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

    inline AliHLTTPCCASliceOutput(): fNTracks( 0 ), fNTrackClusters( 0 ), fTracks( 0 ), fClusterIDrc( 0 ), fClusterPackedYZ( 0 ), fClusterUnpackedYZ( 0 ), fClusterUnpackedX( 0 ), fClusterPackedAmp( 0 ) {}
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
  private:

    AliHLTTPCCASliceOutput( const AliHLTTPCCASliceOutput& )
        : fNTracks( 0 ), fNTrackClusters( 0 ), fTracks( 0 ), fClusterIDrc( 0 ), fClusterPackedYZ( 0 ), fClusterUnpackedYZ( 0 ), fClusterUnpackedX( 0 ), fClusterPackedAmp( 0 ) {}

    const AliHLTTPCCASliceOutput& operator=( const AliHLTTPCCASliceOutput& ) const { return *this; }

    int fNTracks;                 // number of reconstructed tracks
    int fNTrackClusters;          // total number of track clusters
    AliHLTTPCCASliceTrack *fTracks; // pointer to reconstructed tracks
    DataCompressor::RowCluster *fClusterIDrc;  // pointer to cluster IDs ( packed IRow and ICluster)
    unsigned short *fClusterPackedYZ;     // pointer to packed cluster YZ coordinates
    float2   *fClusterUnpackedYZ;   // pointer to cluster coordinates (temporary data, for debug proposes)
    float    *fClusterUnpackedX;   // pointer to cluster coordinates (temporary data, for debug proposes)
    UChar_t  *fClusterPackedAmp;    // pointer to packed cluster amplitudes
    char fMemory[1]; // the memory where the pointers above point into
};

#endif
