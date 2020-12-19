//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCATracker.h,v 1.3 2012/08/20 14:12:59 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCATRACKER_H
#define ALIHLTTPCCATRACKER_H


#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCAHit.h"
#include <iostream>
#include "AliHLTArray.h"
#include "AliHLTTPCCATrackletVector.h"
#include "AliHLTTPCCATrack.h"
#include "AliHLTTPCCAStartHitId.h"
#include "AliHLTTPCCAOutTrack.h"
#include <cstdio>
#include "AliHLTTPCCASliceDataVector.h"
#include <vector>

#include "AliHLTTPCCASliceOutput.h"

class AliHLTTPCCATrack;
class AliHLTTPCCATrackParam;
class AliHLTTPCCAClusterData;

/**
 * @class AliHLTTPCCATracker
 *
 * Slice tracker for ALICE HLT.
 * The class reconstructs tracks in one slice of TPC.
 * The reconstruction algorithm is based on the Cellular Automaton method
 *
 * The CA tracker is designed stand-alone.
 * It is integrated to the HLT framework via AliHLTTPCCATrackerComponent interface.
 * The class is under construction.
 *
 */
class AliHLTTPCCATracker
{
  friend class AliHLTTPCCAPerformance;
  public:
    class NeighboursFinder;
    class NeighboursCleaner;
    class StartHitsFinder;
    class Reconstructor;

    AliHLTTPCCATracker();

    ~AliHLTTPCCATracker();

    void Initialize( const AliHLTTPCCAParam &param );

    void StartEvent();

    void ReadEvent( AliHLTTPCCAClusterData *clusterData );

    void Reconstruct();
    void WriteOutput();

    void GetErrors2( int iRow,  const AliHLTTPCCATrackParam &t, float *Err2Y, float *Err2Z ) const;
    void GetErrors2( int iRow,  const AliHLTTPCCATrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const;
    void GetErrors2( const uint_v &rowIndexes, const AliHLTTPCCATrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const;

    void RecalculateHitsSize( int MaxNHits );
    void SetPointersHits( int MaxNHits );
    void RecalculateTrackMemorySize( int MaxNTracks, int MaxNHits );
    void SetPointersTracks( int MaxNTracks, int MaxNHits );

    void WriteTracks( std::ostream &out ) ;
    void ReadTracks( std::istream &in );

    int ISlice() const { return fParam.ISlice(); }
    float SliceAlpha() const { return fParam.Alpha(); }
    const AliHLTTPCCAParam &Param() const { return fParam; }

    double Timer( int i ) const { return fTimers[i]; }

    const SliceData &Data() const { return fData; }
    const AliHLTTPCCAClusterData &ClusterData() const { return *fClusterData; }

    int NTracklets() const { return fNTracklets; }
    int  *NTracklets() { return &fNTracklets; }  
    void SetNTracklets(int nTrlets) { fNTracklets = nTrlets; }

    const AliHLTTPCCAStartHitId &TrackletStartHit( int i ) const { return fTrackletStartHits[i]; }
    Vc::vector<AliHLTTPCCAStartHitId>& TrackletStartHits() { return fTrackletStartHits; }
    const Vc::vector<AliHLTTPCCAStartHitId>& TrackletStartHits() const { return fTrackletStartHits; }

    size_t NTracks() const { return fTracks.size(); }
    const std::vector<AliHLTTPCCATrack *> &Tracks() const { return fTracks; }

    const AliHLTTPCCASliceOutput * Output() const { return fOutput; }
    AliHLTTPCCASliceOutput * Output() { return fOutput; }

//#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    int fNOutTracks1; // number of tracks in fOutTracks array
    AliHLTTPCCAOutTrack *fOutTracks1; // output array of the reconstructed tracks

    int NOutTracks1() const { return fNOutTracks1; }
    AliHLTTPCCAOutTrack *OutTracks1() const { return  fOutTracks1; }
    const AliHLTTPCCAOutTrack &OutTrack1( int index ) const { return fOutTracks1[index]; }
//#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
    void StoreToFile( FILE *f ) const;
    void RestoreFromFile( FILE *f );

  private:
    void SetupCommonMemory();

#ifdef TETA
    void ConvertPTrackParamToVector( const AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV);
#endif

    AliHLTTPCCAParam fParam; // parameters
    double fTimers[11]; // running CPU time for different parts of the algorithm

    /** A pointer to the ClusterData object that the SliceData was created from. This can be used to
     * merge clusters from inside the SliceTracker code and recreate the SliceData. */
    AliHLTTPCCAClusterData *fClusterData; // ^
    AliHLTTPCCASliceData fData; // The SliceData object. It is used to encapsulate the storage in memory from the access

    // event

    char *fHitMemory; // event memory for hits
    int   fHitMemorySize; // size of the event memory [bytes]

    char *fTrackMemory; // event memory for tracks
    int   fTrackMemorySize; // size of the event memory [bytes]


    Vc::vector<AliHLTTPCCAStartHitId> fTrackletStartHits;   // start hits for the tracklets

    int fNTracklets;     // number of tracklets
    AliHLTResizableArray<TrackletVector> fTrackletVectors; // tracklet data

    //
    int fNumberOfTracks;
    std::vector<AliHLTTPCCATrack *> fTracks;  // reconstructed tracks

    int fNTrackHits; // number of track hits

    // output

    AliHLTTPCCASliceOutput *fOutput;

  private:
    AliHLTTPCCATracker( const AliHLTTPCCATracker& );
    AliHLTTPCCATracker &operator=( const AliHLTTPCCATracker& );
};

#ifdef TETA
struct TrSort {
  int nHits, trId;
  bool operator() ( const TrSort& a, const TrSort& b ) { return (a.nHits > b.nHits);}
  static bool trComp( const TrSort& a, const TrSort& b ) { return (a.nHits > b.nHits); }
};

inline void AliHLTTPCCATracker::ConvertPTrackParamToVector( const AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV)
{
  float_v tmpFloat;
  int_v tmpShort;

  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->X();
  t.SetX(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->SignCosPhi();
  t.SetSignCosPhi(tmpFloat);

  for(int iP=0; iP<5; iP++)
  {
    for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Par()[iP];
    t.SetPar(iP,tmpFloat);
  }
  for(int iC=0; iC<15; iC++)
  {
    for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Cov()[iC];
    t.SetCov(iC,tmpFloat);
  }
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Chi2();
  t.SetChi2(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpShort[iV] = t0[iV]->NDF();
  t.SetNDF(tmpShort);
}
#endif

inline void AliHLTTPCCATracker::GetErrors2( int iRow, const AliHLTTPCCATrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  fParam.GetClusterErrors2( iRow, t, Err2Y, Err2Z );
}
inline void AliHLTTPCCATracker::GetErrors2( const uint_v &rowIndexes, const AliHLTTPCCATrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  fParam.GetClusterErrors2( rowIndexes, t, Err2Y, Err2Z );
}
typedef AliHLTTPCCATracker Tracker;

#endif
