//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCASLICETRACKVECTOR_H
#define ALIHLTTPCCASLICETRACKVECTOR_H

#include "AliHLTTPCCATrackParamVector.h"

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
class AliHLTTPCCASliceTrackVector
{
  public:
    AliHLTTPCCASliceTrackVector()
      : fFirstClusterRef(0)
      , fNClusters(0)
      , fInnerRow(0)
      , fOuterRow(0)
      , fUsed(0)
      , fActive()
#if 0
      , fInnerAlpha(0.f)
      , fOuterAlpha(0.f)
      , fInnerParam()
      , fOuterParam()
#endif
  {}
#if 0
    AliHLTTPCCASliceTrackVector( int_v firstcl, int_v ncl, int_v innrow, int_v outrow, int_v used, int_m act, AliHLTTPCCATrackParamVector innpar )
      : fFirstClusterRef(firstcl)
      , fNClusters(ncl)
      , fInnerRow(innrow)
      , fOuterRow(outrow)
      , fUsed(used)
      , fActive(act)
      , fInnerAlpha(0.f)
      , fOuterAlpha(0.f)
      , fInnerParam(innpar)
      , fOuterParam()
  {}
#endif

    int_v NClusters()                    const { return fNClusters;       }
    int_v FirstClusterRef()              const { return fFirstClusterRef; }

    void SetNClusters( int_v v )                   { fNClusters = v;       }
    void SetNClusters( int i, int v )                   { fNClusters[i] = v;       }
    void SetFirstClusterRef( int_v v )              { fFirstClusterRef = v; }
    void SetFirstClusterRef( int i, int n )              { fFirstClusterRef[i] = n; }

    int_v Used()         const { return fUsed;            }
    int_m Active()	 const { return fActive;	  }
#if 0
    const AliHLTTPCCATrackParamVector &InnerParam() const { return fInnerParam;      }
    const AliHLTTPCCATrackParamVector &OuterParam() const { return fOuterParam;      }
    float_v InnerAlpha() const { return fInnerAlpha;      }
    float_v OuterAlpha() const { return fOuterAlpha;      }
    void SetInnerParam( const AliHLTTPCCATrackParamVector &v )	{ fInnerParam = v;      }
    void SetOuterParam( const AliHLTTPCCATrackParamVector &v )	{ fOuterParam = v;      }
    void SetInnerAlpha( float_v v )				{ fInnerAlpha = v;      }
    void SetInnerAlpha( int i, float v )				{ fInnerAlpha[i] = v;      }
    void SetOuterAlpha( float_v v )				{ fOuterAlpha = v;      }
#endif
    void SetUsed( int_v v )					{ fUsed = v;            }
    void SetUsed( int i, int n )				{ fUsed[i] = n;         }
    void SetActive( int_m m )					{ fActive = m; 		}
    void SetActive( int i, bool m )				{ fActive[i] = m; 	}

    void SetInnerRow( int i, int n ) { fInnerRow[i] = n; }

    void SetNSegments( int n, int iv ) { fNSegments[iv] = n; }
    int NSegments( int iv ) { return fNSegments[iv]; }
    int NSegments( int iv ) const { return fNSegments[iv]; }
    void SetNClustersSeg( int s, int n, int iv )                   { fNClustersSeg[n][iv] = s;       }
    void SetFirstClusterRefSeg( int s, int n, int iv )              { fFirstClusterRefSeg[n][iv] = s; }
    int NClustersSeg( int n, int iv )                    const { return fNClustersSeg[n][iv];       }
    int FirstClusterRefSeg( int n, int iv )              const { return fFirstClusterRefSeg[n][iv]; }
    void SetSliceSeg( int s, int n, int iv )                   { fSliceSeg[n][iv] = s;       }
    int SliceSeg( int n, int iv )                    const { return fSliceSeg[n][iv];       }

  private:

//    AliHLTTPCCATrackParamVector fParam; //* fitted track parameters at its innermost cluster
    int_v fFirstClusterRef;       //* index of the index of the first track cluster in corresponding cluster arrays
    int_v fNClusters;             //* number of track clusters

    int_v fNSegments;
    int_v fFirstClusterRefSeg[2];       //* index of the index of the first track cluster in corresponding cluster arrays
    int_v fNClustersSeg[2];             //* number of track clusters
    int_v fSliceSeg[2];             //* number of track clusters

    int_v fInnerRow; // number of the inner row of the track
    int_v fOuterRow; // number of the outer row of the track
    int_v fUsed;            // is the slice track already merged (=1 -> merged, 0 -> not merged)
    int_m fActive;
#if 0
    float_v fInnerAlpha;               // The angle of the sector, where inner parameters are
    float_v fOuterAlpha;               // The angle of the sector, where outers parameters are
    AliHLTTPCCATrackParamVector fInnerParam; // Parameters of the track at the inner point
    AliHLTTPCCATrackParamVector fOuterParam; // Parameters of the track at the outer point
#endif

};


#endif
