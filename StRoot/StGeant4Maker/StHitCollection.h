#ifndef __StHitCollection_h__
#define __StHitCollection_h__

class DetectorHit;
class TrackerHit;
class CalorimeterHit;

#include <TNamed.h>
#include <TVector.h>
#include <vector>
#include <map>
#include <iostream>

class TVirtualMCStack;

/**
 * @class StHitCollection
 * @brief An abstract base class for collections of simulation hits.
 *
 * This class defines the common interface for hit collections from different
 * detector types. It provides virtual methods for initialization, processing
 * hits during an event, and end-of-event actions.
 */
class StHitCollection : public TNamed {

public:

  StHitCollection( const char* name, const char* title );
 ~StHitCollection(){ /* nada */ };

  virtual void Initialize()  = 0;
  virtual void ProcessHits() = 0;
  virtual void EndOfEvent()  = 0;
  virtual void Clear(Option_t* o="")       = 0;

  virtual int  numberOfHits()  = 0;

  void SetUserStack( TVirtualMCStack* stack ) { mUserStack = stack; }

private:
protected:

  TVirtualMCStack* mUserStack;
  
  ClassDef(StHitCollection,0);

};

/**
 * @class StTrackerHitCollection
 * @brief A collection of hits for tracking detectors.
 *
 * This class manages hits generated in tracking detectors. Each hit (`TrackerHit`)
 * stores detailed information about a particle's passage through a sensitive
 * volume, including entry and exit points, momentum, and energy loss.
 */
class StTrackerHitCollection : public StHitCollection {

public:
  StTrackerHitCollection( const char* name, const char* title );
 ~StTrackerHitCollection() { /* nada */ };

  virtual void Initialize();
  virtual void ProcessHits();
  virtual void EndOfEvent();
  virtual void Clear(Option_t* o="");

  virtual int numberOfHits(){ return mHits.size(); } 

  std::vector<TrackerHit*>& hits(){ return mHits; }

private:
protected:

  std::vector<TrackerHit*> mHits;

  ClassDef(StTrackerHitCollection,0);

};

 
/**
 * @class StCalorimeterHitCollection
 * @brief A collection of hits for calorimeter detectors.
 *
 * This class manages hits in calorimeter detectors. It accumulates energy
 * depositions (`CalorimeterHit`) within a sensitive volume. At the end of
 * an event, it aggregates hits from the same volume into a single summary hit.
 * It also applies a correction for the Birk's law saturation effect.
 */
class StCalorimeterHitCollection : public StHitCollection {

public:
  StCalorimeterHitCollection( const char* name, const char* title );
 ~StCalorimeterHitCollection() { /* nada */ };

  virtual void Initialize();
  virtual void ProcessHits();
  virtual void EndOfEvent();
  virtual void Clear(Option_t* o="");

  virtual int numberOfHits(){ return mHits.size(); } 

  std::vector<CalorimeterHit*>& hits(){ return mHits; }

private:
protected:

  std::vector<CalorimeterHit*> mHits;
  double mBirk[3];
  double mEsum;

  ClassDef(StCalorimeterHitCollection,0);

};

std::ostream&  operator<<(std::ostream& os,  const TrackerHit& hit);

#endif
