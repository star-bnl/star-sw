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

      tRevers = false;
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
    void SetRevers( bool r = true ) { tRevers = r; }
    bool IsRevers() const { return tRevers; }
  
  private:

    AliHLTTPCCATrackParam fInnerParam; //* fitted track parameters at the TPC inner radius
    AliHLTTPCCATrackParam fOuterParam; //* fitted track parameters at the TPC outer radius
    float fInnerAlpha;               //* alpha angle for the inner parameters
    float fOuterAlpha;               //* alpha angle for the outer parameters
    int fFirstClusterRef;            //* index of the first track cluster in corresponding cluster arrays
    int fNClusters;                  //* number of track clusters

    int fPrevSegment;
    int fNextSegment;
    bool tIsMerged;
    bool fUsed;

    bool fIsLooper;
    bool fIsGrow;

    bool tRevers;
};


#endif
