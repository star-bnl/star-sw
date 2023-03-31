#include "StRHICfPoint.h"

ClassImp(StRHICfPoint)

StRHICfPoint::StRHICfPoint()
{
  clear();
}

StRHICfPoint::~StRHICfPoint()
{
}

void StRHICfPoint::clear()
{
  mTowerIdx = -999;
  mParticleID = -999;

  memset(mPointPos, 0, sizeof(mPointPos));
  memset(mPointEnergy, 0, sizeof(mPointEnergy));
  memset(mTowerSumEnergy, 0, sizeof(mTowerSumEnergy));
}

void StRHICfPoint::setTowerIdx(Int_t val){mTowerIdx = val;}
void StRHICfPoint::setPID(Int_t pid){mParticleID = pid;}
void StRHICfPoint::setPointPos(Float_t x, Float_t y)
{
  mPointPos[0] = x;
  mPointPos[1] = y;
}

void StRHICfPoint::setPointEnergy(Float_t pid1, Float_t pid2)
{
  mPointEnergy[0] = pid1;
  mPointEnergy[1] = pid2;
}

void StRHICfPoint::setTowerSumEnergy(Float_t all, Float_t part)
{
  mTowerSumEnergy[0] = all;
  mTowerSumEnergy[1] = part;
}

Int_t StRHICfPoint::getTowerIdx(){return mTowerIdx;}
Int_t StRHICfPoint::getPID(){return mParticleID;}
Float_t StRHICfPoint::getPointPos(Int_t xy){return mPointPos[xy];}
Float_t StRHICfPoint::getPointEnergy(Int_t particle){return mPointEnergy[particle];}
Float_t StRHICfPoint::getTowerSumEnergy(Int_t order){return mTowerSumEnergy[order];}
