//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCAMERGEDTRACK_H
#define ALIHLTTPCCAMERGEDTRACK_H

#include "AliHLTTPCCAMerger.h"
#include "AliHLTTPCCATrackParam.h"

/**
 * @class AliHLTTPCCAMergedTrack
 * AliHLTTPCCAMergedTrack class is used to store TPC tracks,
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
class AliHLTTPCCAMergedTrack
{
  public:

    int NClusters()                         const { return fNClusters;       }
    int FirstClusterRef()                   const { return fFirstClusterRef; }
    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam;      }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam;      }
    float InnerAlpha()                      const { return fInnerAlpha;      }
    float OuterAlpha()                      const { return fOuterAlpha;      }

    void SetNClusters      ( int v )                  { fNClusters = v;       }
    void SetFirstClusterRef( int v )                  { fFirstClusterRef = v; }
    void SetInnerParam( const AliHLTTPCCATrackParam &v ) { fInnerParam = v;      }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) { fOuterParam = v;      }
    void SetInnerAlpha( float v )                       { fInnerAlpha = v;      }
    void SetOuterAlpha( float v )                       { fOuterAlpha = v;      }
    void AssignTrack( const AliHLTTPCCAMerger::AliHLTTPCCASliceTrackInfo& t, int firstClusterRef  ) {
      fInnerParam = t.InnerParam();
      fOuterParam = t.OuterParam();
      fInnerAlpha = t.InnerAlpha();
      fOuterAlpha = t.OuterAlpha();
      fFirstClusterRef = firstClusterRef;
      fNClusters = t.NClusters();
      // ---
      tIsMerged = false;
      fUsed = t.Used();

      fIsLooper = false;
      fIsGrow = false;
      fPrevSegment = -5;
      fNextSegment = -5;
      // ---
    }
    // ---
    void SetMerged() { tIsMerged = true; }
    bool IsMerged() const { return tIsMerged; }

    void SetUsed() { fUsed = true; }
    void SetNoUsed() { fUsed = false; }
    bool Used() const { return fUsed; }

    void SetLooper( int prev, int next ) {
      fIsLooper = true;
      fPrevSegment = prev;
      fNextSegment = next;
    }
    void SetNoLooper() {
      fIsLooper = false;
      fIsGrow = false;
      fPrevSegment = -5;
      fNextSegment = -5;
    }
    bool IsLooper() { return fIsLooper; }
    bool IsLooper() const { return fIsLooper; }
    int LpPrevNb() const { return fPrevSegment; }
    int LpNextNb() const { return fNextSegment; }
    void SetGrow() { fIsGrow = true; }
    bool IsGrow() const { return fIsGrow; }
    // ---
  
  private:

    AliHLTTPCCATrackParam fInnerParam; //* fitted track parameters at the TPC inner radius
    AliHLTTPCCATrackParam fOuterParam; //* fitted track parameters at the TPC outer radius
    float fInnerAlpha;               //* alpha angle for the inner parameters
    float fOuterAlpha;               //* alpha angle for the outer parameters
    int fFirstClusterRef;            //* index of the first track cluster in corresponding cluster arrays
    int fNClusters;                  //* number of track clusters
    // ---
    bool tIsMerged;
    bool fUsed;

    bool fIsLooper;
    bool fIsGrow;
    int fPrevSegment;
    int fNextSegment;
    // ---
};


#endif
