#ifndef __StiTPCCATrackerInterface_h__
#define __StiTPCCATrackerInterface_h__
#ifdef DO_TPCCATRACKER
#include "StiTpcSeedFinder.h"
#include "TPCCATracker/code/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/code/AliHLTTPCCAPerformance.h"
#include "TPCCATracker/code/AliHLTTPCCAMCTrack.h"

#include "StiTrackContainer.h"

class StiTPCCATrackerInterface {
 public:

    /// Instance
  static StiTPCCATrackerInterface &Instance();
  
    /// constructor - destructor
  StiTPCCATrackerInterface();
  ~StiTPCCATrackerInterface();

  void SetNewEvent(); // clean and initialize before new event

  
  void SetHits(HitMapToVectorAndEndType &map_){ fHitsMap = &map_; };// set hits data array.
  void SetStiTracks( StiTrackContainer *fStiTracks_){ fStiTracks = fStiTracks_; };

  void Run();                                                    // copy data to CATracker, run CATracker, copy tracks in fSeeds. Should be called after SetHits(...).
  vector<Seed_t> &GetSeeds(){ return fSeeds; };                   // get seeds. Should be called after Run(...).

  void RunPerformance();
  
  vector<SeedHit_t>        GetSeedHits()    { return fSeedHits;}
  

 private:
  typedef AliHLTTPCCAPerformance::AliHLTTPCCAHitLabel AliHLTTPCCAHitLabel;

  
  void MakeSettings(); // fill fCaParam
  void MakeHits();     // fill fCaHits & fSeedHits
  void MakeSeeds();    // fill fSeeds & fTrackParameters
  void FillPerformance(const vector<AliHLTTPCCAGBHit>& hits, const vector<int>& idTruth, vector<AliHLTTPCCAMCTrack>& mcTracks, vector<AliHLTTPCCALocalMCPoint>& mcPoints, vector<AliHLTTPCCAHitLabel>& hitLabels); // fill fPerformance by MCTracks, MCPoints and Hit-MCPointsMatch

  void ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StiNodePars& nodePars, StiNodeErrs& nodeErrs); // convert caPars into NodePars


  HitMapToVectorAndEndType *fHitsMap;
  vector<Seed_t> fSeeds;

  StiTpcSeedFinder *fSeedFinder;
  AliHLTTPCCAGBTracker *fTracker;

  vector<int> fIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAMCTrack> fMCTracks;
  vector<AliHLTTPCCALocalMCPoint> fMCPoints;
  vector<AliHLTTPCCAHitLabel> fHitLabels;
  AliHLTTPCCAPerformance *fPerformance;


  vector<AliHLTTPCCAParam> fCaParam;// settings for all sectors to give CATracker
  vector<AliHLTTPCCAGBHit> fCaHits; // hits to give CATracker
  vector<SeedHit_t> fSeedHits;          // hits to make seeds

  TFile *fOutFile; // file for perfo histos
  
    // for StiPefro
  void FillStiPerformance();

  AliHLTTPCCAGBTracker *fStiTracker;
  
  StiTrackContainer *fStiTracks;
  vector<AliHLTTPCCAGBHit> fStiCaHits; // hits to give CATracker.
  vector<int> fStiIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAGBTrack> fStiCaTracks;

  AliHLTResizableArray<AliHLTTPCCAHitLabel> fStiHitLabels;
  AliHLTResizableArray<AliHLTTPCCAMCTrack> fStiMCTracks;
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> fStiMCPoints;


  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
};
#endif /* DO_TPCCATRACKER */
#endif //  __StiTPCCATrackerInterface_h__
