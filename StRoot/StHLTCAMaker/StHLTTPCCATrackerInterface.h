#ifndef __StHLTTPCCATrackerInterface_h__
#define __StHLTTPCCATrackerInterface_h__
#include "StiCA/StiCATpcSeedFinder.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/StTPCCAInterface.h"
#include "Sti/StiTrackContainer.h"

class StHLTTPCCATrackerInterface : public StTPCCAInterface {
 public:
  StHLTTPCCATrackerInterface();
  virtual ~StHLTTPCCATrackerInterface() {fgStHLTTPCCATrackerInterface = 0;}
  static StHLTTPCCATrackerInterface &Instance();
  virtual void SetNewEvent() {fSeedFinder = 0; fSeeds.clear(); fSeedHits.clear(); fHitsMap = 0; StTPCCAInterface::SetNewEvent();}
  virtual void SetHits(HitMapToVectorAndEndType &map_){ fHitsMap = &map_; };// set hits data array.
  virtual vector<Seed_t> &GetSeeds(){ return fSeeds; };                   // get seeds. Should be called after Run(...).
  virtual vector<SeedHit_t>        GetSeedHits()    { return fSeedHits;}

 protected:
  virtual void MakeHits();     // fill fCaHits & fSeedHits
  virtual void MakeSeeds();    // fill fSeeds & fTrackParameters
  virtual void ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StiNodePars& nodePars, StiNodeErrs& nodeErrs); // convert caPars into NodePars

  HitMapToVectorAndEndType *fHitsMap;
  vector<Seed_t>            fSeeds;
  StiCATpcSeedFinder       *fSeedFinder;
  vector<SeedHit_t>         fSeedHits;          // hits to make seeds
  //  online_tracking_TpcHitMap* hitMap[nTpcSectors]; // STAR HLT TPC hit map 
  void HLTHitG2L(const int iSector, const int iPadrow, const double globalXyz[3], double HLTLocalXyz[3]);
  static StHLTTPCCATrackerInterface *fgStHLTTPCCATrackerInterface;
};
#endif //  __StHLTTPCCATrackerInterface_h__
