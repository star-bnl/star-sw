#ifndef __StiCATpcTrackerInterface_h__
#define __StiCATpcTrackerInterface_h__
#include "StiCATpcSeedFinder.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"

#include "Sti/StiTrackContainer.h"

class StiCATpcTrackerInterface {
 public:
  static StiCATpcTrackerInterface &Instance() {static StiCATpcTrackerInterface g; return g;}
  StiCATpcTrackerInterface() {}
  ~StiCATpcTrackerInterface() {}
  void SetNewEvent(); // clean and initialize before new event
  void SetHits(HitMapToVectorAndEndType &map_){ fHitsMap = &map_; };// set hits data array.
  void Run();                                                    // copy data to CATracker, run CATracker, copy tracks in fSeeds. Should be called after SetHits(...).
  vector<Seed_t> &GetSeeds(){ return fSeeds; };                   // get seeds. Should be called after Run(...).
  void RunPerformance();
  vector<SeedHit_t>        GetSeedHits()    { return fSeedHits;}

 protected:
  void MakeSettings(); // fill fCaParam
  virtual void MakeHits();     // fill fCaHits & fSeedHits
  void MakeSeeds();    // fill fSeeds & fTrackParameters
  void ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StiNodePars& nodePars, StiNodeErrs& nodeErrs); // convert caPars into NodePars

  HitMapToVectorAndEndType *fHitsMap;
  vector<Seed_t>            fSeeds;
  StiCATpcSeedFinder       *fSeedFinder;
  AliHLTTPCCAGBTracker     *fTracker;
  vector<int>               fIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAParam>  fCaParam;// settings for all sectors to give CATracker
  vector<AliHLTTPCCAGBHit>  fCaHits; // hits to give CATracker
  vector<SeedHit_t>         fSeedHits;          // hits to make seeds
  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
};
#endif //  __StiCATpcTrackerInterface_h__
