#include "StRHICfSimRHICfPoint.h"

ClassImp(StRHICfSimRHICfPoint)

StRHICfSimRHICfPoint::StRHICfSimRHICfPoint()
{
    Clear();
}

StRHICfSimRHICfPoint::~StRHICfSimRHICfPoint()
{
}

void StRHICfSimRHICfPoint::Clear(Option_t *option)
{

    mTowerIdx = -999;
    mParticleID = -999;

    fill_n(&mPointPos[0], rXYNum, -999.);
    fill_n(&mPointEnergy[0], 2, -999.);
}

void StRHICfSimRHICfPoint::SetTowerIdx(int val){mTowerIdx = val;}
void StRHICfSimRHICfPoint::SetPID(int pid){mParticleID = pid;}

void StRHICfSimRHICfPoint::SetPointPos(float x, float y)
{
    mPointPos[0] = x;
    mPointPos[1] = y;
}

void StRHICfSimRHICfPoint::SetPointEnergy(float pid1, float pid2)
{
    mPointEnergy[0] = pid1;
    mPointEnergy[1] = pid2;
}

Int_t StRHICfSimRHICfPoint::GetTowerIdx(){return mTowerIdx;}
Int_t StRHICfSimRHICfPoint::GetPID(){return mParticleID;}
Float_t StRHICfSimRHICfPoint::GetPointPos(int xy){return mPointPos[xy];}
Float_t StRHICfSimRHICfPoint::GetPointEnergy(int particle){return mPointEnergy[particle];}