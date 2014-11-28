// -*- C++ -*-
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// Jan 26, 2008
//

#ifndef ST_EEMC_SHOWER_SHAPE_H
#define ST_EEMC_SHOWER_SHAPE_H

// C++ STL
#include <vector>
using std::vector;

// ROOT
#include "TClonesArray.h"
#include "TVector3.h"

// STAR
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"

class StEEmcShowerShape : public TObject {
public:
  StEEmcShowerShape();
  StEEmcShowerShape(const StEEmcShowerShape&);
  ~StEEmcShowerShape();

  int runNumber() const;
  int eventNumber() const;
  float energy() const;
  float preshower1() const;
  float preshower2() const;
  float postshower() const;
  int sector() const;
  int numberOfUstrips() const;
  int numberOfVstrips() const;
  StMuEmcHit* uStrip(int i) const;
  StMuEmcHit* vStrip(int i) const;
  int highUstripId() const;
  int highVstripId() const;
  TVector3 position() const;
  TVector3 momentum() const;

  void Clear(Option_t* option = "");
  void setRunNumber(int runNumber);
  void setEventNumber(int evenNumber);
  void setEnergy(float energy);
  void setPreshower1(float preshower1);
  void setPreshower2(float preshower2);
  void setPostshower(float postshower);
  void setSector(int sector);
  void addUstrip(StMuEmcHit* strip);
  void addVstrip(StMuEmcHit* strip);
  void setHighUstripId(int id);
  void setHighVstripId(int id);
  void setPosition(const TVector3& v);
  void setMomentum(const TVector3& v);

private:
  // Disable assignment operator
  StEEmcShowerShape& operator=(const StEEmcShowerShape&);

  int mEventNumber;
  int mRunNumber;
  float mEnergy;
  float mPreshower1;
  float mPreshower2;
  float mPostshower;
  int mSector;
  TClonesArray* mUstrips;
  TClonesArray* mVstrips;
  int mHighUstripId;
  int mHighVstripId;
  TVector3 mPosition;
  TVector3 mMomentum;

  ClassDef(StEEmcShowerShape, 1);
};

inline int StEEmcShowerShape::runNumber() const { return mRunNumber; }
inline int StEEmcShowerShape::eventNumber() const { return mEventNumber; }
inline float StEEmcShowerShape::energy() const { return mEnergy; }
inline float StEEmcShowerShape::preshower1() const { return mPreshower1; }
inline float StEEmcShowerShape::preshower2() const { return mPreshower2; }
inline float StEEmcShowerShape::postshower() const { return mPostshower; }
inline int StEEmcShowerShape::sector() const { return mSector; }
inline int StEEmcShowerShape::numberOfUstrips() const { return mUstrips->GetEntriesFast(); }
inline int StEEmcShowerShape::numberOfVstrips() const { return mVstrips->GetEntriesFast(); }
inline StMuEmcHit* StEEmcShowerShape::uStrip(int i) const { return (StMuEmcHit*)mUstrips->At(i); }
inline StMuEmcHit* StEEmcShowerShape::vStrip(int i) const { return (StMuEmcHit*)mVstrips->At(i); }
inline int StEEmcShowerShape::highUstripId() const { return mHighUstripId; }
inline int StEEmcShowerShape::highVstripId() const { return mHighVstripId; }
inline TVector3 StEEmcShowerShape::position() const { return mPosition; }
inline TVector3 StEEmcShowerShape::momentum() const { return mMomentum; }

inline void StEEmcShowerShape::Clear(Option_t* option)
{
  mUstrips->Clear(option);
  mVstrips->Clear(option);
}

inline void StEEmcShowerShape::setRunNumber(int runNumber) { mRunNumber = runNumber; }
inline void StEEmcShowerShape::setEventNumber(int eventNumber) { mEventNumber = eventNumber; }
inline void StEEmcShowerShape::setEnergy(float energy) { mEnergy = energy; }
inline void StEEmcShowerShape::setPreshower1(float preshower1) { mPreshower1 = preshower1; }
inline void StEEmcShowerShape::setPreshower2(float preshower2) { mPreshower2 = preshower2; }
inline void StEEmcShowerShape::setPostshower(float postshower) { mPostshower = postshower; }
inline void StEEmcShowerShape::setSector(int sector) { mSector = sector; }
inline void StEEmcShowerShape::addUstrip(StMuEmcHit* strip) { new ((*mUstrips)[mUstrips->GetEntriesFast()]) StMuEmcHit(*strip); }
inline void StEEmcShowerShape::addVstrip(StMuEmcHit* strip) { new ((*mVstrips)[mVstrips->GetEntriesFast()]) StMuEmcHit(*strip); }
inline void StEEmcShowerShape::setHighUstripId(int id) { mHighUstripId = id; }
inline void StEEmcShowerShape::setHighVstripId(int id) { mHighVstripId = id; }
inline void StEEmcShowerShape::setPosition(const TVector3& v) { mPosition = v; }
inline void StEEmcShowerShape::setMomentum(const TVector3& v) { mMomentum = v; }

ostream& operator<<(ostream& out, StMuEmcHit& hit);
ostream& operator<<(ostream& out, const StEEmcShowerShape&);

#endif // ST_EEMC_SHOWER_SHAPE_H
