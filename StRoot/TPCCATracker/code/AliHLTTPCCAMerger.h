//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAMERGER_H
#define ALIHLTTPCCAMERGER_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCATrackParam.h"

#if !defined(HLTCA_GPUCODE)
#include <iostream>
#endif

class AliHLTTPCCASliceTrack;
class AliHLTTPCCASliceOutput;
class AliHLTTPCCAMergedTrack;
class AliHLTTPCCAMergerOutput;

/**
 * @class AliHLTTPCCAMerger
 *
 */
class AliHLTTPCCAMerger
{
///mvz start
class AliHLTTPCCAClusterInfo;
///mvz end
  private:
    class AliHLTTPCCAClusterInfo;
    class AliHLTTPCCASliceTrackInfo;
    class AliHLTTPCCABorderTrackGlobal;

  public:

    AliHLTTPCCAMerger();
    ~AliHLTTPCCAMerger();

    void SetSliceParam( const AliHLTTPCCAParam &v ) { fSliceParam = v; }

    void Clear();
    void SetSliceData( int index, const AliHLTTPCCASliceOutput *SliceData );
    void Reconstruct();
    const AliHLTTPCCAMergerOutput * Output() const { return fOutput; }

    bool FitTrack( AliHLTTPCCATrackParam &T, float &Alpha,
                     AliHLTTPCCATrackParam t0, float Alpha0, int hits[], int &NHits,  bool dir = 0 );

    void SetSlices ( int i, AliHLTTPCCATracker *sl );

    void CalculateHelix(AliHLTTPCCABorderTrackGlobal *bLong, float z, float &x, float &y, float &dx2, float &dy2);
    void CalculateR(const float &x1, const float &y1, const float &x2, const float &y2, const float &x3, const float &y3, float &R);
    void CalculatedRToPoint(AliHLTTPCCABorderTrackGlobal *b, const float &x, const float &y, const float &dx2, const float &dy2, float &R, float &dR);

/*    void CalculateR(const float &x1, const float &y1, const float &x2, const float &y2, const float &x3, const float &y3, 
                    const float &dx1, const float &dy1, const float &dx2, const float &dy2, const float &dx3, const float &dy3, 
                    float &R, float &dR2);*/

  private:

    AliHLTTPCCAMerger( const AliHLTTPCCAMerger& );
    const AliHLTTPCCAMerger &operator=( const AliHLTTPCCAMerger& ) const;

    void MakeBorderTracksGlobal(AliHLTTPCCABorderTrackGlobal B[], int &nB );
    void SplitBorderTracksGlobal( AliHLTTPCCABorderTrackGlobal B1[], int N1, AliHLTTPCCABorderTrackGlobal B2[], int N2, int number);

    static float GetChi2( float x1, float y1, float a00, float a10, float a11, float x2, float y2, float b00, float b10, float b11);

    void UnpackSlices();
    void Merging(int number=0);

    static const int fgkNSlices = AliHLTTPCCAParameters::NumberOfSlices;       //* N slices
    AliHLTTPCCAParam fSliceParam;           //* slice parameters (geometry, calibr, etc.)
    const AliHLTTPCCASliceOutput *fkSlices[fgkNSlices]; //* array of input slice tracks
    AliHLTTPCCAMergerOutput *fOutput;       //* array of output merged tracks
    AliHLTTPCCASliceTrackInfo *fTrackInfos; //* additional information for slice tracks
    int fMaxTrackInfos;                   //* booked size of fTrackInfos array
    AliHLTTPCCAClusterInfo *fClusterInfos;  //* information about track clusters
    int fMaxClusterInfos;                 //* booked size of fClusterInfos array
    int fSliceTrackInfoStart[fgkNSlices];   //* slice starting index in fTrackInfos array;
    int fSliceNTrackInfos[fgkNSlices];                //* N of slice track infos in fTrackInfos array;

    AliHLTTPCCATracker *slices[fgkNSlices]; //* array of input slice tracks
};

#endif
