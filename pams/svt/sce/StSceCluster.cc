#include "StSceCluster.hh"

StSceCluster::StSceCluster(int rNCluster)
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

StSceCluster::StSceCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, int rTotNoise, float rStripMean, int rFlag, int *rIdMcHit)
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

StSceCluster::StSceCluster(const StSceCluster & originalCluster)
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

StSceCluster::~StSceCluster()
{  delete [] mIdMcHit; }

StSceCluster& StSceCluster::operator=(const StSceCluster originalCluster)
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

void  StSceCluster::setNCluster(int rNCluster)
{  mNCluster = rNCluster; }

void  StSceCluster::setFirstStrip(int rFirstStrip)
{  mFirstStrip = rFirstStrip; }

void  StSceCluster::setClusterSize(int rClusterSize)
{  mClusterSize = rClusterSize; }

void  StSceCluster::setTotAdc(int rTotAdc)
{  mTotAdc = rTotAdc; }

void  StSceCluster::setFirstAdc(int rFirstAdc)
{  mFirstAdc = rFirstAdc; }

void  StSceCluster::setLastAdc(int rLastAdc)
{  mLastAdc = rLastAdc; }

void  StSceCluster::setTotNoise(int rTotNoise)
{  mTotNoise = rTotNoise; }

void  StSceCluster::setStripMean(float rStripMean)
{  mStripMean = rStripMean; }

void  StSceCluster::setFlag(int rFlag)
{  mFlag = rFlag; }

void StSceCluster::setIdMcHit(int rIdMcHit, int iR)
{  mIdMcHit[iR] = rIdMcHit; }


void StSceCluster::setPrevCluster(StSceCluster *rPrevCluster)
{  mPrevCluster = rPrevCluster; }

void StSceCluster::setNextCluster(StSceCluster *rNextCluster)
{  mNextCluster = rNextCluster; }


int StSceCluster::getNCluster()
{  return mNCluster; }

int StSceCluster::getFirstStrip()
{  return mFirstStrip; }

int StSceCluster::getClusterSize()
{  return mClusterSize; }

int StSceCluster::getTotAdc()
{  return mTotAdc; }

int StSceCluster::getFirstAdc()
{  return mFirstAdc; }

int StSceCluster::getLastAdc()
{  return mLastAdc; }

int StSceCluster::getTotNoise()
{  return mTotNoise; }

float StSceCluster::getStripMean()
{  return mStripMean; }

int  StSceCluster::getFlag()
{  return mFlag; }

int StSceCluster::getIdMcHit(int iR)
{  return mIdMcHit[iR]; }


StSceCluster* StSceCluster::getPrevCluster()
{  return mPrevCluster;}

StSceCluster* StSceCluster::getNextCluster()
{  return mNextCluster;}


StSceCluster* StSceCluster::giveCopy()
{
  StSceCluster *ptrClone = new StSceCluster(this->mNCluster);
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


