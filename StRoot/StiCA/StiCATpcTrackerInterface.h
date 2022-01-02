#ifndef __StiCATpcTrackerInterface_h__
#define __StiCATpcTrackerInterface_h__
#include "StiCATpcSeedFinder.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/StTPCCAInterface.h"
#include "Sti/StiTrackContainer.h"

class StiCATpcTrackerInterface : public StTPCCAInterface {
 public:
 StiCATpcTrackerInterface() : StTPCCAInterface() {}
  virtual ~StiCATpcTrackerInterface() {fgStiCATpcTrackerInterface = 0;}
  static StiCATpcTrackerInterface &Instance();
  virtual void SetNewEvent();
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
  static StiCATpcTrackerInterface *fgStiCATpcTrackerInterface;
  static Bool_t             fgUseCAVxFinder;
};
#endif //  __StiCATpcTrackerInterface_h__
