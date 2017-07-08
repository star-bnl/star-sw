#ifndef __StxInterface_h__
#define __StxInterface_h__
#include <map>
#include "StxSeedFinder.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
#include "TPCCATrackerPerformance/AliHLTTPCCAPerformance.h"
#include "TPCCATrackerPerformance/AliHLTTPCCAMCTrack.h"
#endif
#include "StEvent.h"
#include "StTpcHit.h"
class StxInterface {
 public:
  static StxInterface &Instance();
  StxInterface();
  ~StxInterface() {}
  void SetNewEvent(); // clean and initialize before new event
#if 0  
  void SetStxTracks( StxTrackContainer *fStxTracks_){ fStxTracks = fStxTracks_; };
#endif
  void Run(StEvent *event=0);
  vector<Seed_t> &GetSeeds() { return *&fSeeds; }
  void RunPerformance();
 protected:
  void MakeSettings(); // fill fCaParam
  virtual void MakeHits(StEvent *event=0);     // fill fCaHits & fSeedHits
  void MakeSeeds();    // fill fSeeds & fTrackParameters
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  void FillPerformance(const vector<AliHLTTPCCAGBHit>& hits, const vector<int>& idTruth, vector<AliHLTTPCCAMCTrack>& mcTracks, vector<AliHLTTPCCALocalMCPoint>& mcPoints, vector<AliHLTTPCCAHitLabel>& hitLabels); // fill fPerformance by MCTracks, MCPoints and Hit-MCPointsMatch
#endif
#if 0
  void ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StxNodePars& nodePars, StxNodeErrs& nodeErrs); // convert caPars into NodePars
  HitMapToVectorAndEndType *fHitsMap;
#endif
  vector<Seed_t> fSeeds;
  StxSeedFinder *fSeedFinder;
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
  map<UInt_t,const StTpcHit*> fSeedHits;          // hits to make seeds
  void FillStxPerformance();
  vector<AliHLTTPCCAGBHit> fStxCaHits; // hits to give CATracker.
  vector<int> fStxIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAGBTrack> fStxCaTracks;
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  AliHLTResizableArray<AliHLTTPCCAHitLabel> fStxHitLabels;
  AliHLTResizableArray<AliHLTTPCCAMCTrack> fStxMCTracks;
  AliHLTResizableArray<AliHLTTPCCALocalMCPoint> fStxMCPoints;
#endif
  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
};
#endif //  __StxInterface_h__
