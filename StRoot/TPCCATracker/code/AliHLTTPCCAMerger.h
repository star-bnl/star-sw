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
    static void SetDoNotMergeBorders(int i = 0) {fgDoNotMergeBorders = i;}

  private:
  
    void InvertCholetsky(float a[15]);
    void MultiplySS(float const C[15], float const V[15], float K[5][5]);
    void MultiplyMS(float const C[5][5], float const V[15], float K[15]);
    void MultiplySR(float const C[15], float const r_in[5], float r_out[5]);
    void FilterTracks(float const r[5], float const C[15], float const m[5], float const V[15], float R[5], float W[15], float &chi2);

    AliHLTTPCCAMerger( const AliHLTTPCCAMerger& );
    const AliHLTTPCCAMerger &operator=( const AliHLTTPCCAMerger& ) const;

    void MakeBorderTracksGlobal(AliHLTTPCCABorderTrackGlobal B[], int &nB );
    void SplitBorderTracksGlobal( AliHLTTPCCABorderTrackGlobal B1[], int N1, AliHLTTPCCABorderTrackGlobal B2[], int N2, int number);

    static float GetChi2( float x1, float y1, float a00, float a10, float a11, float x2, float y2, float b00, float b10, float b11);

    void UnpackSlices();
    void Merging(int number=0);

    static const int fgkNSlices = AliHLTTPCCAParameters::NumberOfSlices;       //* N slices
    static       int fgDoNotMergeBorders;
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
