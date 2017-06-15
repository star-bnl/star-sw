#ifndef __StiCATpcTrackerInterface_h__
#define __StiCATpcTrackerInterface_h__
#include "StiCATpcSeedFinder.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
#include "TPCCATrackerPerformance/AliHLTTPCCAPerformance.h"
#include "TPCCATrackerPerformance/AliHLTTPCCAMCTrack.h"
#endif

#include "Sti/StiTrackContainer.h"

class StiCATpcTrackerInterface {
 public:

    /// Instance
  static StiCATpcTrackerInterface &Instance();
  
    /// constructor - destructor
  StiCATpcTrackerInterface();
  ~StiCATpcTrackerInterface();

  void SetNewEvent(); // clean and initialize before new event

  
  void SetHits(HitMapToVectorAndEndType &map_){ fHitsMap = &map_; };// set hits data array.
  void SetStiTracks( StiTrackContainer *fStiTracks_){ fStiTracks = fStiTracks_; };

  void Run();                                                    // copy data to CATracker, run CATracker, copy tracks in fSeeds. Should be called after SetHits(...).
  vector<Seed_t> &GetSeeds(){ return fSeeds; };                   // get seeds. Should be called after Run(...).

  void RunPerformance();
  
  vector<SeedHit_t>        GetSeedHits()    { return fSeedHits;}
  

 protected:
  
  void MakeSettings(); // fill fCaParam
  virtual void MakeHits();     // fill fCaHits & fSeedHits
  void MakeSeeds();    // fill fSeeds & fTrackParameters
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  void FillPerformance(const vector<AliHLTTPCCAGBHit>& hits, const vector<int>& idTruth, vector<AliHLTTPCCAMCTrack>& mcTracks, vector<AliHLTTPCCALocalMCPoint>& mcPoints, vector<AliHLTTPCCAHitLabel>& hitLabels); // fill fPerformance by MCTracks, MCPoints and Hit-MCPointsMatch
#endif
  void ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StiNodePars& nodePars, StiNodeErrs& nodeErrs); // convert caPars into NodePars

  HitMapToVectorAndEndType *fHitsMap;
  vector<Seed_t> fSeeds;

  StiCATpcSeedFinder *fSeedFinder;
  AliHLTTPCCAGBTracker *fTracker;

  vector<int> fIdTruth; // id of the Track, which has created CaHit
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  vector<AliHLTTPCCAMCTrack> fMCTracks;
  vector<AliHLTTPCCALocalMCPoint> fMCPoints;
  vector<AliHLTTPCCAHitLabel> fHitLabels;
  AliHLTTPCCAPerformance *fPerformance;

  TFile *fOutFile; // file for perfo histos
#endif

  vector<AliHLTTPCCAParam> fCaParam;// settings for all sectors to give CATracker
  vector<AliHLTTPCCAGBHit> fCaHits; // hits to give CATracker
  vector<SeedHit_t> fSeedHits;          // hits to make seeds

    // for StiPefro
  void FillStiPerformance();

  AliHLTTPCCAGBTracker *fStiTracker;
  
  StiTrackContainer *fStiTracks;
  vector<AliHLTTPCCAGBHit> fStiCaHits; // hits to give CATracker.
  vector<int> fStiIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAGBTrack> fStiCaTracks;

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  AliHLTResizableArray<AliHLTTPCCAHitLabel> fStiHitLabels;
  AliHLTResizableArray<AliHLTTPCCAMCTrack> fStiMCTracks;
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> fStiMCPoints;
#endif
  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
};
#endif //  __StiCATpcTrackerInterface_h__
