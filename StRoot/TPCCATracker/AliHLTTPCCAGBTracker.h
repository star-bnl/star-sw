//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAGBTRACKER_H
#define ALIHLTTPCCAGBTRACKER_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCATracker.h"

#include <cstdio>
#include <iostream>
#include <vector>
#include <string>
using std::string;

class AliHLTTPCCAMerger;

/**
 * @class AliHLTTPCCAGBTracker
 *
 * Global Cellular Automaton-based HLT tracker for TPC detector
 * The class reconstructs tracks in the whole TPC
 * It calls the AliHLTTPCCATracker slice tracker and constructs
 * the global TPC tracks by merging the slice tracks
 *
 * The tracker is designed stand-alone.
 * It will be integrated to the HLT framework via AliHLTTPCCAGBTrackerComponent interface,
 * and to off-line framework via TPC/AliTPCtrackerCA class
 * The class is under construction.
 *
 */
class AliHLTTPCCAGBTracker
{

  public:
    AliHLTTPCCAGBTracker();
    ~AliHLTTPCCAGBTracker();
    void Init();

    void StartEvent();
    void SetNSlices( int N );
    void SetNHits( int nHits );

    void FindTracks();

    void Merge();

    AliHLTArray<AliHLTTPCCATracker> Slices() const { return fSlices; }
    const AliHLTTPCCATracker &Slice( int index ) const { return fSlices[index]; }
    const AliHLTTPCCAGBHit *Hits() const { return fHits.Data(); }
    const AliHLTTPCCAGBHit &Hit( int index ) const { return fHits[index]; }
    int Ext2IntHitID( int i ) const { return fExt2IntHitID[i]; }
    int NHits() const { return fNHits; }
    int NSlices() const { return fNSlices; }
    double Time() const { return fTime; }
    double StatTime( int iTimer ) const { return fStatTime[iTimer]; }
    int NTimers() const { return fNTimers; }
    int StatNEvents() const { return fStatNEvents; }
    int NTracks() const { return fNTracks; }
    AliHLTTPCCAGBTrack *Tracks() const { return fTracks; }
    AliHLTTPCCAGBTrack *Tracks() { return fTracks; }
    const AliHLTTPCCAGBTrack &Track( int i ) const { return fTracks[i]; }
    int *TrackHits() const { return fTrackHits; }
    int *TrackHits() { return fTrackHits; }
    int TrackHit( int i ) const { return fTrackHits[i]; }

    bool FitTrack( AliHLTTPCCATrackParam &T, AliHLTTPCCATrackParam t0,
                     float &Alpha, int hits[], int &NTrackHits,
                     bool dir );

    void WriteSettings( std::ostream &out ) const;
    void ReadSettings( std::istream &in );
    void WriteEvent( FILE *out ) const;
    void ReadEvent( FILE *in );
    void WriteTracks( const string& prefix ) const;
    void ReadTracks( std::istream &in );

    void SaveHitsInFile( string prefix ) const; // Save Hits in txt file. @prefix - prefix for file name. Ex: "./data/ev1"
    void SaveSettingsInFile( string prefix ) const; // Save geometry in txt file. @prefix - prefix for file name. Ex: "./data/"
    bool ReadHitsFromFile( string prefix );
    bool ReadSettingsFromFile( string prefix );

    double SliceTrackerTime() const { return fSliceTrackerTime; }
    double SliceTrackerCpuTime() const { return fSliceTrackerCpuTime; }
    const int *FirstSliceHit() const { return fFirstSliceHit; }
    int FirstSliceHit(int i) const { return fFirstSliceHit[i]; }

    void StoreToFile( const char *filename ) const;
    void RestoreFromFile( FILE *f );
    
    void SetHits( const std::vector<AliHLTTPCCAGBHit> &hits);     // need for StRoot
    void SetHits( const AliHLTTPCCAGBHit *hits, int nHits );      // for CA_parallel
    void SetSettings( const std::vector<AliHLTTPCCAParam>& settings ); // need for StRoot
    int  GetHitsSize() const {return fHits.Size();}

      /// Try to group close hits in row formed by one track. After sort hits.
//     void GroupHits(); // iklm
  friend class AliHLTTPCCAPerformance; //dbg
  protected:

    AliHLTResizableArray<AliHLTTPCCATracker> fSlices; //* array of slice trackers
    int fNSlices;              //* N slices
    AliHLTResizableArray<AliHLTTPCCAGBHit> fHits;     //* hit array
    int *fExt2IntHitID;        //* array of internal hit indices
    int fNHits;                //* N hits in event
    int *fTrackHits;           //* track->hits reference array
    AliHLTTPCCAGBTrack *fTracks; //* array of tracks
    int fNTracks;              //* N tracks
    AliHLTTPCCAMerger *fMerger;  //* global merger

    AliHLTResizableArray<AliHLTTPCCAClusterData, 1, AliHLTFullyCacheLineAligned> fClusterData;

    double fTime; //* total time
    static const int fNTimers = 25;
    double fStatTime[fNTimers]; //* timers
    int fStatNEvents;    //* n events proceed
    int fFirstSliceHit[100]; // hit array

    double fSliceTrackerTime; // reco time of the slice tracker;
    double fSliceTrackerCpuTime; // reco time of the slice tracker;

  private:
    AliHLTTPCCAGBTracker( const AliHLTTPCCAGBTracker& );
    AliHLTTPCCAGBTracker &operator=( const AliHLTTPCCAGBTracker& );
};

#endif
