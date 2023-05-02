#ifndef StRHICfPoint_hh
#define StRHICfPoint_hh

#include <TObject.h>
#include "StEnumerations.h"

class StRHICfPoint : public TObject 
{
  public:
    StRHICfPoint();
    ~StRHICfPoint();

    void clear();

    void setTowerIdx(Int_t val);
    void setPID(Int_t pid);
    void setPointPos(Float_t x, Float_t y);
    void setPointEnergy(Float_t pid1, Float_t pid2);
    void setTowerSumEnergy(Float_t all, Float_t part);

    Int_t getTowerIdx();
    Int_t getPID();
    Float_t getPointPos(Int_t xy);
    Float_t getPointEnergy(Int_t particle);
    Float_t getTowerSumEnergy(Int_t order);

  private:
    Int_t mTowerIdx;
    Int_t mParticleID;

    Float_t mPointPos[kRHICfNxy];
    Float_t mPointEnergy[kRHICfNorder];
    Float_t mTowerSumEnergy[kRHICfNorder];

  ClassDef(StRHICfPoint,1)
};

#endif
