#include "StMuRHICfPoint.h"

ClassImp(StMuRHICfPoint)

StMuRHICfPoint::StMuRHICfPoint()
{
  clear();
}

StMuRHICfPoint::~StMuRHICfPoint()
{
}

void StMuRHICfPoint::clear()
{
  mTowerIdx = -999;
  mParticleID = -999;

  memset(mPointPos, 0, sizeof(mPointPos));
  memset(mPointEnergy, 0, sizeof(mPointEnergy));
  memset(mTowerSumEnergy, 0, sizeof(mTowerSumEnergy));
}

void StMuRHICfPoint::setTowerIdx(Int_t val){mTowerIdx = val;}
void StMuRHICfPoint::setPID(Int_t pid){mParticleID = pid;}
void StMuRHICfPoint::setPointPos(Float_t x, Float_t y)
{
  mPointPos[0] = x;
  mPointPos[1] = y;
}

void StMuRHICfPoint::setPointEnergy(Float_t pid1, Float_t pid2)
{
  mPointEnergy[0] = pid1;
  mPointEnergy[1] = pid2;
}

void StMuRHICfPoint::setTowerSumEnergy(Float_t all, Float_t part)
{
  mTowerSumEnergy[0] = all;
  mTowerSumEnergy[1] = part;
}

Int_t StMuRHICfPoint::getTowerIdx(){return mTowerIdx;}
Int_t StMuRHICfPoint::getPID(){return mParticleID;}
Float_t StMuRHICfPoint::getPointPos(Int_t xy){return mPointPos[xy];}
Float_t StMuRHICfPoint::getPointEnergy(Int_t particle){return mPointEnergy[particle];}
Float_t StMuRHICfPoint::getTowerSumEnergy(Int_t order){return mTowerSumEnergy[order];}
