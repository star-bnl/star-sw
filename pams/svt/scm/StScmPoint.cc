#include "StScmPoint.hh"
#include <stdiostream.h> // a enlever 

StScmPoint::StScmPoint(int rNPoint, int rNWafer, int rNumPackage, int rKindPackage)
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

StScmPoint::StScmPoint(const StScmPoint & originalPoint)
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

StScmPoint::~StScmPoint()
{
 delete [ ] mNMchit;
 delete [ ] mDe;
 delete [ ] mPositionU;
 delete [ ] mXg;
 delete [ ] mXl;
}

StScmPoint& StScmPoint::operator=(const StScmPoint originalPoint)
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

void  StScmPoint::setFlag(int rFlag)
{  mFlag = rFlag; }

void  StScmPoint::setNPoint(int rNPoint)
{  mNPoint = rNPoint; }

void  StScmPoint::setNCluster(int rNCluster)
{  mNCluster = rNCluster; }

void  StScmPoint::setNMatched(int rNMatched)
{  mNMatched = rNMatched; }

void  StScmPoint::setNMchit(int rNMchit, int iR)
{  mNMchit[iR] = rNMchit; }

void  StScmPoint::setNWafer(int rNWafer)
{  mNWafer = rNWafer; }


void  StScmPoint::setEnergyLoss(float adcP, float adcN)
{
  setDe((adcP + adcN)/2.,0);
  setDe((adcP - adcN)/2.,1);
 }

void  StScmPoint::setDe(float rDe, int iR)
{   mDe[iR] = rDe; }

void  StScmPoint::setPositionU(float rPositionU, int iR)
{  mPositionU[iR] = rPositionU; }

void  StScmPoint::setXg(float rXg, int iR)
{  mXg[iR] = rXg; }

void  StScmPoint::setXl(float rXl, int iR)
{  mXl[iR] = rXl; }


void StScmPoint::setPrevPoint(StScmPoint *rPrevPoint)
{  mPrevPoint = rPrevPoint; }

void StScmPoint::setNextPoint(StScmPoint *rNextPoint)
{  mNextPoint = rNextPoint; }


int StScmPoint::getFlag()
{  return mFlag; }

int StScmPoint::getNPoint()
{  return mNPoint; }

int StScmPoint::getNCluster()
{  return mNCluster; }

int StScmPoint::getNMatched()
{  return mNMatched; }

int StScmPoint::getNMchit(int iR)
{  return mNMchit[iR]; }

int StScmPoint::getNWafer()
{  return mNWafer; }

float StScmPoint::getDe(int iR)
{  return mDe[iR]; }

float StScmPoint::getPositionU(int iR)
{  return mPositionU[iR]; }

float StScmPoint::getXg(int iR)
{  return mXg[iR]; }

float StScmPoint::getXl(int iR)
{  return mXl[iR]; }


StScmPoint* StScmPoint::getPrevPoint()
{  return mPrevPoint; }

StScmPoint* StScmPoint::getNextPoint()
{  return mNextPoint; }


StScmPoint* StScmPoint::giveCopy()
{
  StScmPoint *ptrClone = new StScmPoint(this->mNPoint, this->mNWafer, this-> mNCluster, this->mNMatched);
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
