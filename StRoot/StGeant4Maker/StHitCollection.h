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

class StHitCollection : public TNamed {

public:

  StHitCollection( const char* name, const char* title );
 ~StHitCollection(){ /* nada */ };

  virtual void Initialize()  = 0;
  virtual void ProcessHits() = 0;
  virtual void EndOfEvent()  = 0;
  virtual void Clear()       = 0;

  virtual int  numberOfHits()  = 0;

private:
protected:
  
  ClassDef(StHitCollection,0);

};

class StTrackerHitCollection : public StHitCollection {

public:
  StTrackerHitCollection( const char* name, const char* title );
 ~StTrackerHitCollection() { /* nada */ };

  virtual void Initialize();
  virtual void ProcessHits();
  virtual void EndOfEvent();
  virtual void Clear();

  virtual int numberOfHits(){ return mHits.size(); } 

  std::vector<TrackerHit*>& hits(){ return mHits; }

private:
protected:

  std::vector<TrackerHit*> mHits;

  ClassDef(StTrackerHitCollection,0);

};

 
class StCalorimeterHitCollection : public StHitCollection {

public:
  StCalorimeterHitCollection( const char* name, const char* title );
 ~StCalorimeterHitCollection() { /* nada */ };

  virtual void Initialize();
  virtual void ProcessHits();
  virtual void EndOfEvent();
  virtual void Clear();

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
