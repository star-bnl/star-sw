#include "StSsdPoint.hh"

StSsdPoint::StSsdPoint(int rNPoint, int rNWafer, int rNumPackage, int rKindPackage)
{
  mFlag           = 0;
  mNPoint         = rNPoint;
  mNCluster       = rNumPackage;
  mNMatched       = rKindPackage;
  mNMchit         = new int[5];
  mNWafer         = rNWafer;
  mPositionU      = new float[2];
  mXg             = new float[3];
  mXl             = new float[3];
  mDe             = new float[2];

  int i = 0;
  for (i = 0; i < 5; i++)
      mNMchit[i]    = 0;

  for (i = 0; i < 3; i++)
    {
      mXg[i]        = 0.;
      mXl[i]        = 0.;
    }

  for (i = 0; i < 2; i++)
    {
      mPositionU[i] = 0.;
      mDe[i]        = 0.;
    }
}

StSsdPoint::StSsdPoint(const StSsdPoint & originalPoint)
{
  mFlag           = originalPoint.mFlag;
  mNPoint         = originalPoint.mNPoint;
  mNCluster       = originalPoint.mNCluster;
  mNMatched       = originalPoint.mNMatched;
  mNMchit         = new int[5];
  mNWafer         = originalPoint.mNWafer;
  mPositionU      = new float[2];
  mXg             = new float[3];
  mXl             = new float[3];
  mDe             = new float[2];

  int i = 0;
  for (i = 0; i < 5; i++)
      mNMchit[i]    = originalPoint.mNMchit[i];

  for (i = 0; i < 3; i++)
    {
      mXg[i]        = originalPoint.mXg[i];
      mXl[i]        = originalPoint.mXl[i];
    }

  for (i = 0; i < 2; i++)
    {
      mPositionU[i] = originalPoint.mPositionU[i];
      mDe[i]        = originalPoint.mDe[i];
    }
}

StSsdPoint::~StSsdPoint()
{
 delete [ ] mNMchit;
 delete [ ] mDe;
 delete [ ] mPositionU;
 delete [ ] mXg;
 delete [ ] mXl;
}

StSsdPoint& StSsdPoint::operator=(const StSsdPoint originalPoint)
{
  this->mFlag           = originalPoint.mFlag;
  this->mNPoint         = originalPoint.mNPoint;
  this->mNCluster       = originalPoint.mNCluster;
  this->mNMatched       = originalPoint.mNMatched;
  this->mNWafer         = originalPoint.mNWafer;

  int i = 0;
  for (i = 0; i < 5; i++)
      this->mNMchit[i]    = originalPoint.mNMchit[i];

  for (i = 0; i < 3; i++)
    {
      this->mXg[i]        = originalPoint.mXg[i];
      this->mXl[i]        = originalPoint.mXl[i];
    }

  for (i = 0; i < 2; i++)
    {
      this->mPositionU[i] = originalPoint.mPositionU[i];
      this->mDe[i]        = originalPoint.mDe[i];
    }
  return *this;
} 

void  StSsdPoint::setFlag(int rFlag)
{  mFlag = rFlag; }

void  StSsdPoint::setNPoint(int rNPoint)
{  mNPoint = rNPoint; }

void  StSsdPoint::setNCluster(int rNCluster)
{  mNCluster = rNCluster; }

void  StSsdPoint::setNMatched(int rNMatched)
{  mNMatched = rNMatched; }

void  StSsdPoint::setNMchit(int rNMchit, int iR)
{  mNMchit[iR] = rNMchit; }

void  StSsdPoint::setNWafer(int rNWafer)
{  mNWafer = rNWafer; }


void  StSsdPoint::setEnergyLoss(float adcP, float adcN)
{
  setDe((adcP + adcN)/2.,0);
  setDe((adcP - adcN)/2.,1);
 }

void  StSsdPoint::setDe(float rDe, int iR)
{   mDe[iR] = rDe; }

void  StSsdPoint::setPositionU(float rPositionU, int iR)
{  mPositionU[iR] = rPositionU; }

void  StSsdPoint::setXg(float rXg, int iR)
{  mXg[iR] = rXg; }

void  StSsdPoint::setXl(float rXl, int iR)
{  mXl[iR] = rXl; }


void StSsdPoint::setPrevPoint(StSsdPoint *rPrevPoint)
{  mPrevPoint = rPrevPoint; }

void StSsdPoint::setNextPoint(StSsdPoint *rNextPoint)
{  mNextPoint = rNextPoint; }


int StSsdPoint::getFlag()
{  return mFlag; }

int StSsdPoint::getNPoint()
{  return mNPoint; }

int StSsdPoint::getNCluster()
{  return mNCluster; }

int StSsdPoint::getNMatched()
{  return mNMatched; }

int StSsdPoint::getNMchit(int iR)
{  return mNMchit[iR]; }

int StSsdPoint::getNWafer()
{  return mNWafer; }

float StSsdPoint::getDe(int iR)
{  return mDe[iR]; }

float StSsdPoint::getPositionU(int iR)
{  return mPositionU[iR]; }

float StSsdPoint::getXg(int iR)
{  return mXg[iR]; }

float StSsdPoint::getXl(int iR)
{  return mXl[iR]; }


StSsdPoint* StSsdPoint::getPrevPoint()
{  return mPrevPoint; }

StSsdPoint* StSsdPoint::getNextPoint()
{  return mNextPoint; }


StSsdPoint* StSsdPoint::giveCopy()
{
  StSsdPoint *ptrClone = new StSsdPoint(this->mNPoint, this->mNWafer, this-> mNCluster, this->mNMatched);
  int i = 0;
  ptrClone->mFlag = this->mFlag;
  for (i = 0; i < 5; i++)
      ptrClone->mNMchit[i]   = this->mNMchit[i];

  for (i = 0; i < 3; i++)
    { ptrClone->mXg[i]         = this->mXg[i];
      ptrClone->mXl[i]         = this->mXl[i];
    }
  for (i = 0; i < 2; i++)
    { ptrClone->mPositionU[i]  = this->mPositionU[i];
      ptrClone->mDe[i]         = this->mDe[i];
    }
  return ptrClone;
}
