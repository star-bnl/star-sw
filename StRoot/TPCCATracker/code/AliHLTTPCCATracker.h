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
#include "AliHLTTPCCAHitId.h"
#include "AliHLTTPCCAOutTrack.h"
#include <cstdio>
#include "AliHLTTPCCASliceDataVector.h"
#include <vector>

class AliHLTTPCCATrack;
class AliHLTTPCCATrackParam;
class AliHLTTPCCAClusterData;
class AliHLTTPCCASliceOutput;

//X using Vc::int_v;
//X using Vc::uint_v;
//X using Vc::float_v;
//X using Vc::Mask;

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
    void GetErrors2( int iRow,  const AliHLTTPCCATrackParamVector &t, sfloat_v *Err2Y, sfloat_v *Err2Z ) const;
    void GetErrors2( const ushort_v &rowIndexes, const AliHLTTPCCATrackParamVector &t, sfloat_v *Err2Y, sfloat_v *Err2Z ) const;
///mvz start 20.01.2010
/*  void GetErrors2( int iRow, float z, float sinPhi, float cosPhi, float DzDs, float *Err2Y, float *Err2Z ) const;
    void GetErrors2( int iRow, const sfloat_v &z, const sfloat_v &sinPhi, const sfloat_v &DzDs, sfloat_v *Err2Y, sfloat_v *Err2Z ) const;
    void GetErrors2( const ushort_v &rowIndexes, const sfloat_v &z, const sfloat_v &sinPhi, const sfloat_v &DzDs, sfloat_v *Err2Y, sfloat_v *Err2Z ) const;
*/
///mvz end 20.01.2010

    void RecalculateHitsSize( int MaxNHits );
    void SetPointersHits( int MaxNHits );
    void RecalculateTrackMemorySize( int MaxNTracks, int MaxNHits );
    void SetPointersTracks( int MaxNTracks, int MaxNHits );

    void WriteTracks( std::ostream &out ) ;
    void ReadTracks( std::istream &in );

    const AliHLTTPCCAParam &Param() const { return fParam; }

    double Timer( int i ) const { return fTimers[i]; }

    const SliceData &Data() const { return fData; }
    const AliHLTTPCCAClusterData &ClusterData() const { return *fClusterData; }

    int NTracklets() const { return fNTracklets; }
    int  *NTracklets() { return &fNTracklets; }  
    void SetNTracklets(int nTrlets) { fNTracklets = nTrlets; }

    const AliHLTTPCCAHitId &TrackletStartHit( int i ) const { return fTrackletStartHits[i]; }
    AliHLTTPCCAHitId *TrackletStartHits() const { return fTrackletStartHits; }

    size_t NTracks() const { return fTracks.size(); }
    const std::vector<AliHLTTPCCATrack *> &Tracks() const { return fTracks; }

    const AliHLTTPCCASliceOutput * Output() const { return fOutput; }

///mvz start

    int fNOutTracks1; // number of tracks in fOutTracks array
    AliHLTTPCCAOutTrack *fOutTracks1; // output array of the reconstructed tracks

    int NOutTracks1() const { return fNOutTracks1; }
    AliHLTTPCCAOutTrack *OutTracks1() const { return  fOutTracks1; }
    const AliHLTTPCCAOutTrack &OutTrack1( int index ) const { return fOutTracks1[index]; }
///mvz end

    int NOutTracks() const { return fNOutTracks; }
    AliHLTTPCCAOutTrack *OutTracks() const { return  fOutTracks; }
    const AliHLTTPCCAOutTrack &OutTrack( int index ) const { return fOutTracks[index]; }
    int NOutTrackHits() const { return fNOutTrackHits; }
    const int *OutTrackHits() const { return  fOutTrackHits; }
    int OutTrackHit( int i ) const { return  fOutTrackHits[i]; }

    void StoreToFile( FILE *f ) const;
    void RestoreFromFile( FILE *f );

  private:
    void SetupCommonMemory();

    AliHLTTPCCAParam fParam; // parameters
    double fTimers[10]; // running CPU time for different parts of the algorithm

    /** A pointer to the ClusterData object that the SliceData was created from. This can be used to
     * merge clusters from inside the SliceTracker code and recreate the SliceData. */
    AliHLTTPCCAClusterData *fClusterData; // ^
    AliHLTTPCCASliceData fData; // The SliceData object. It is used to encapsulate the storage in memory from the access

    // event

    char *fHitMemory; // event memory for hits
    int   fHitMemorySize; // size of the event memory [bytes]

    char *fTrackMemory; // event memory for tracks
    int   fTrackMemorySize; // size of the event memory [bytes]


    AliHLTTPCCAHitId *fTrackletStartHits;   // start hits for the tracklets

    int fNTracklets;     // number of tracklets
    AliHLTResizableArray<TrackletVector> fTrackletVectors; // tracklet data

    //
    int fNumberOfTracks;
    std::vector<AliHLTTPCCATrack *> fTracks;  // reconstructed tracks

    int fNTrackHits; // number of track hits

    // output

    AliHLTTPCCASliceOutput *fOutput;

    // obsolete output

    int fNOutTracks; // number of tracks in fOutTracks array
    AliHLTTPCCAOutTrack *fOutTracks; // output array of the reconstructed tracks
    int fNOutTrackHits;  // number of hits in fOutTrackHits array
    int *fOutTrackHits;  // output array of ID's of the reconstructed hits

  private:
    AliHLTTPCCATracker( const AliHLTTPCCATracker& );
    AliHLTTPCCATracker &operator=( const AliHLTTPCCATracker& );
};

inline void AliHLTTPCCATracker::GetErrors2( int iRow, const AliHLTTPCCATrackParamVector &t, sfloat_v *Err2Y, sfloat_v *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  fParam.GetClusterErrors2( iRow, t, Err2Y, Err2Z );
}
inline void AliHLTTPCCATracker::GetErrors2( const ushort_v &rowIndexes, const AliHLTTPCCATrackParamVector &t, sfloat_v *Err2Y, sfloat_v *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  fParam.GetClusterErrors2( rowIndexes, t, Err2Y, Err2Z );
}
///mvz start 20.01.2010
/*
inline void AliHLTTPCCATracker::GetErrors2( int iRow, const sfloat_v &z, const sfloat_v &sinPhi,
    const sfloat_v &DzDs, sfloat_v *Err2Y, sfloat_v *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  VALGRIND_CHECK_VALUE_IS_DEFINED( z );
  VALGRIND_CHECK_VALUE_IS_DEFINED( DzDs );

  fParam.GetClusterErrors2( iRow, z, sinPhi, DzDs, *Err2Y, *Err2Z );
}
inline void AliHLTTPCCATracker::GetErrors2( const ushort_v &iRow, const sfloat_v &z, const sfloat_v &sinPhi,
    const sfloat_v &DzDs, sfloat_v *Err2Y, sfloat_v *Err2Z ) const {
  fParam.GetClusterErrors2( iRow, z, sinPhi, DzDs, *Err2Y, *Err2Z );
}*/
///mvz end 20.01.2010
typedef AliHLTTPCCATracker Tracker;

#endif
