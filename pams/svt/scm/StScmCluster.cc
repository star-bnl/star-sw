#include "StScmCluster.hh"

StScmCluster::StScmCluster(int rNCluster)
{
  mNCluster    = rNCluster;
  mFirstStrip  = 769;
  mClusterSize = 0;
  mTotAdc      = 0;
  mFirstAdc    = 0;
  mLastAdc     = 0;
  mTotNoise    = 0;
  mStripMean   = 0;
  mFlag        = 0;
  mIdMcHit     = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = 0;

  mPrevCluster = 0;
  mNextCluster = 0;
}

StScmCluster::StScmCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, int rTotNoise, float rStripMean, int rFlag, int *rIdMcHit)
{
  mNCluster    = rNCluster;
  mFirstStrip  = rFirstStrip;
  mClusterSize = rClusterSize;
  mTotAdc      = rTotAdc;
  mFirstAdc    = rFirstAdc;
  mLastAdc     = rLastAdc;
  mTotNoise    = rTotNoise;
  mStripMean   = rStripMean;
  mFlag        = rFlag;
  mIdMcHit     = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = rIdMcHit[e];

  mPrevCluster = 0;
  mNextCluster = 0;
}

StScmCluster::StScmCluster(const StScmCluster & originalCluster)
{
  mNCluster    = originalCluster.mNCluster;
  mFirstStrip  = originalCluster.mFirstStrip;
  mClusterSize = originalCluster.mClusterSize;
  mTotAdc      = originalCluster.mTotAdc;
  mFirstAdc    = originalCluster.mFirstAdc;
  mLastAdc     = originalCluster.mLastAdc;
  mTotNoise    = originalCluster.mTotNoise;
  mStripMean   = originalCluster.mStripMean;
  mFlag        = originalCluster.mFlag;
  mIdMcHit     = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];

  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
}

StScmCluster::~StScmCluster()
{  delete [] mIdMcHit; }

StScmCluster& StScmCluster::operator=(const StScmCluster originalCluster)
{
  mNCluster    = originalCluster.mNCluster;
  mFirstStrip  = originalCluster.mFirstStrip;
  mClusterSize = originalCluster.mClusterSize;
  mTotAdc      = originalCluster.mTotAdc;
  mFirstAdc    = originalCluster.mFirstAdc;
  mLastAdc     = originalCluster.mLastAdc;
  mTotNoise    = originalCluster.mTotNoise;
  mStripMean   = originalCluster.mStripMean;
  mFlag        = originalCluster.mFlag;
  for(int e=0;e<5;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];

  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
  return *this;
}

void  StScmCluster::setNCluster(int rNCluster)
{  mNCluster = rNCluster; }

void  StScmCluster::setFirstStrip(int rFirstStrip)
{  mFirstStrip = rFirstStrip; }

void  StScmCluster::setClusterSize(int rClusterSize)
{  mClusterSize = rClusterSize; }

void  StScmCluster::setTotAdc(int rTotAdc)
{  mTotAdc = rTotAdc; }

void  StScmCluster::setFirstAdc(int rFirstAdc)
{  mFirstAdc = rFirstAdc; }

void  StScmCluster::setLastAdc(int rLastAdc)
{  mLastAdc = rLastAdc; }

void  StScmCluster::setTotNoise(int rTotNoise)
{  mTotNoise = rTotNoise; }

void  StScmCluster::setStripMean(float rStripMean)
{  mStripMean = rStripMean; }

void  StScmCluster::setFlag(int rFlag)
{  mFlag = rFlag; }

void StScmCluster::setIdMcHit(int rIdMcHit, int iR)
{  mIdMcHit[iR] = rIdMcHit; }


void StScmCluster::setPrevCluster(StScmCluster *rPrevCluster)
{  mPrevCluster = rPrevCluster; }

void StScmCluster::setNextCluster(StScmCluster *rNextCluster)
{  mNextCluster = rNextCluster; }


int StScmCluster::getNCluster()
{  return mNCluster; }

int StScmCluster::getFirstStrip()
{  return mFirstStrip; }

int StScmCluster::getClusterSize()
{  return mClusterSize; }

int StScmCluster::getTotAdc()
{  return mTotAdc; }

int StScmCluster::getFirstAdc()
{  return mFirstAdc; }

int StScmCluster::getLastAdc()
{  return mLastAdc; }

int StScmCluster::getTotNoise()
{  return mTotNoise; }

float StScmCluster::getStripMean()
{  return mStripMean; }

int  StScmCluster::getFlag()
{  return mFlag; }

int StScmCluster::getIdMcHit(int iR)
{  return mIdMcHit[iR]; }


StScmCluster* StScmCluster::getPrevCluster()
{  return mPrevCluster;}

StScmCluster* StScmCluster::getNextCluster()
{  return mNextCluster;}


StScmCluster* StScmCluster::giveCopy()
{
  StScmCluster *ptrClone = new StScmCluster(this->mNCluster);
  ptrClone->mFirstStrip  = this->mFirstStrip;
  ptrClone->mClusterSize = this->mClusterSize;
  ptrClone->mTotAdc      = this->mTotAdc;
  ptrClone->mFirstAdc    = this->mFirstAdc;
  ptrClone->mLastAdc     = this->mLastAdc;
  ptrClone->mTotNoise    = this->mTotNoise;
  ptrClone->mStripMean   = this->mStripMean;
  ptrClone->mFlag        = this->mFlag;
  for(int e=0;e<5;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
  return ptrClone;
}


