#include "StTrackForPool.h"

StTrackForPool::StTrackForPool() 
{
  mId=0;
  mPx=0.0;
  mPy=0.0;
  mPz=0.0;
  mPsuedoRap=0.0;
  mRChiXY = 0.0;
  mRChiZ = 0.0;
  mNTPCPoints=0;
  mCharge = 0;
}

StTrackForPool::StTrackForPool(Double_t px, Double_t py, Double_t pz)
{
  mPx=px;
  mPy=py;
  mPz=pz;
  Double_t p=sqrt(px*px+py*py+pz*pz);
  //mid = track->GetIDNumber();
  mPsuedoRap=0.5*log((p+pz)/(p-pz));
  mId=0;
}

StTrackForPool::~StTrackForPool() {}

void 
StTrackForPool::SetMomentum(Double_t px, Double_t py, Double_t pz) 
{
  mPx=px;
  mPy=py;
  mPz=pz;
}

void 
StTrackForPool::GetMomentum(Double_t& px, Double_t& py, Double_t& pz) 
{
  px=mPx;
  py=mPy;
  pz=mPz;
}

void 
StTrackForPool::GetMomentum(Double_t& p) 
{
  p=sqrt(mPx*mPx+mPy*mPy+mPz*mPz);
}

void 
StTrackForPool::GetPt(Double_t& pt) 
{
  pt=sqrt(mPx*mPx+mPy*mPy);
}

void 
StTrackForPool::GetPseudoRapidity(Double_t& pRap)
{
  pRap=mPsuedoRap;
}

void
StTrackForPool::GetTrackIDNumber(Int_t& id) 
{
  id=mId;
}


void
StTrackForPool::SetTrackIDNumber(Int_t id) 
{
  mId=id;
}


void
StTrackForPool::SetRChiSquaredXY(Double_t chiXY) 
{
   mRChiXY = chiXY;
}

void
StTrackForPool::SetRChiSquaredZ(Double_t chiZ) 
{
   mRChiZ = chiZ;
}

void
StTrackForPool::SetNTPCPoints(Int_t npoints) 
{
    mNTPCPoints=npoints;
}

Double_t
StTrackForPool::GetRChiSquaredXY() 
{
   return mRChiXY;
}

Double_t
StTrackForPool::GetRChiSquaredZ() 
{
   return mRChiZ;
}

Int_t
StTrackForPool::GetNTPCPoints() 
{
    return mNTPCPoints;
}

void 
StTrackForPool::SetCharge(Int_t charge)
{
  mCharge=charge;
}


Int_t 
StTrackForPool::GetCharge()
{
  return mCharge;
}
