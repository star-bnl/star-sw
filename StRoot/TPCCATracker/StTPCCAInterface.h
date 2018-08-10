#ifndef __StTPCCAInterface_h__
#define __StTPCCAInterface_h__
#include "Riostream.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"

#include "Sti/StiTrackContainer.h"

class StTPCCAInterface {
 public:
 StTPCCAInterface() : fTracker(0) {}
  virtual ~StTPCCAInterface() {if (fTracker) {delete fTracker; fTracker = 0;}}
  virtual void SetNewEvent(); // clean and initialize before new event
  virtual void Run(); 
  virtual void RunPerformance();

 protected:
  virtual void MakeSettings(); // fill fCaParam
  virtual void MakeHits() {cout << "Dummy StTPCCAInterface::;MakeHits is called" << endl;}
  virtual void MakeSeeds() {cout << "Dummy StTPCCAInterface::;MakeSeeds is called" << endl;}

  AliHLTTPCCAGBTracker     *fTracker;
  vector<int>               fIdTruth; // id of the Track, which has created CaHit
  vector<AliHLTTPCCAParam>  fCaParam;// settings for all sectors to give CATracker
  vector<AliHLTTPCCAGBHit>  fCaHits; // hits to give CATracker
  double fPreparationTime_real, fPreparationTime_cpu; // time for coping data and performance
};
#endif //  __StTPCCAInterface_h__
