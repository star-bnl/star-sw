//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCASLICETRACK_H
#define ALIHLTTPCCASLICETRACK_H

#include "AliHLTTPCCATrackParam.h"

/**
 * @class AliHLTTPCCASliceTrack
 * AliHLTTPCCASliceTrack class is used to store TPC tracks,
 * which are reconstructed by the TPCCATracker slice tracker.
 *
 * The class contains:
 * - fitted track parameters at its first row, the covariance matrix, \Chi^2, NDF (number of degrees of freedom )
 * - n of clusters assigned to the track
 * - index of its first cluster in corresponding cluster arrays
 *
 * The class is used to transport the data between AliHLTTPCCATracker{Component} and AliHLTTPCCAGBMerger{Component}
 *
 */
class AliHLTTPCCASliceTrack
{
  public:
    AliHLTTPCCASliceTrack(): fParam(), fFirstClusterRef(0), fNClusters(0) {}
    int NClusters()                    const { return fNClusters;       }
    int FirstClusterRef()              const { return fFirstClusterRef; }
    const AliHLTTPCCATrackParam &Param() const { return fParam;           }

    void SetNClusters( int v )                   { fNClusters = v;       }
    void SetFirstClusterRef( int v )              { fFirstClusterRef = v; }
    void SetParam( const AliHLTTPCCATrackParam &v ) { fParam = v;           }

  private:

    AliHLTTPCCATrackParam fParam; //* fitted track parameters at its innermost cluster
    int fFirstClusterRef;       //* index of the index of the first track cluster in corresponding cluster arrays
    int fNClusters;             //* number of track clusters

};


#endif
