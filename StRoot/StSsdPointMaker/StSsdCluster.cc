#include "StSsdCluster.hh"

StSsdCluster::StSsdCluster(int rNCluster)
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
  mIdMcHit     = new int[MAXIDMCHIT];
  for(int e=0;e<MAXIDMCHIT;e++)  mIdMcHit[e] = 0;
  mPrevCluster = 0;
  mNextCluster = 0;
}

StSsdCluster::StSsdCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, int rTotNoise, float rStripMean, int rFlag, int *rIdMcHit)
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
  mIdMcHit     = new int[MAXIDMCHIT];
  for(int e=0;e<MAXIDMCHIT;e++)  mIdMcHit[e] = rIdMcHit[e];
  mPrevCluster = 0;
  mNextCluster = 0;
}

StSsdCluster::StSsdCluster(const StSsdCluster & originalCluster)
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
  mIdMcHit     = new int[MAXIDMCHIT];
  for(int e=0;e<MAXIDMCHIT;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];
  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
}

StSsdCluster::~StSsdCluster()
{  delete [] mIdMcHit; }

StSsdCluster& StSsdCluster::operator=(const StSsdCluster originalCluster)
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
  for(int e=0;e<MAXIDMCHIT;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];
  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
  return *this;
}

void  StSsdCluster::setNCluster(int rNCluster)
{ mNCluster = rNCluster;}

void  StSsdCluster::setFirstStrip(int rFirstStrip)
{ mFirstStrip = rFirstStrip;}

void  StSsdCluster::setClusterSize(int rClusterSize)
{ mClusterSize = rClusterSize;}

void  StSsdCluster::setTotAdc(int rTotAdc)
{ mTotAdc = rTotAdc;}

void  StSsdCluster::setFirstAdc(int rFirstAdc)
{ mFirstAdc = rFirstAdc;}

void  StSsdCluster::setLastAdc(int rLastAdc)
{ mLastAdc = rLastAdc;}

void  StSsdCluster::setTotNoise(int rTotNoise)
{ mTotNoise = rTotNoise;}

void  StSsdCluster::setStripMean(float rStripMean)
{ mStripMean = rStripMean;}

void  StSsdCluster::setFlag(int rFlag)
{ mFlag = rFlag;}

void  StSsdCluster::setIdMcHit(int rIdMcHit, int iR)
{ mIdMcHit[iR] = rIdMcHit; }

void StSsdCluster::setPrevCluster(StSsdCluster *rPrevCluster)
{ mPrevCluster = rPrevCluster;}

void StSsdCluster::setNextCluster(StSsdCluster *rNextCluster)
{ mNextCluster = rNextCluster;}


int StSsdCluster::getNCluster()
{  return mNCluster;}

int StSsdCluster::getFirstStrip()
{  return mFirstStrip;}

int StSsdCluster::getClusterSize()
{  return mClusterSize;}

int StSsdCluster::getTotAdc()
{  return mTotAdc;}

int StSsdCluster::getFirstAdc()
{  return mFirstAdc;}

int StSsdCluster::getLastAdc()
{  return mLastAdc;}

int StSsdCluster::getTotNoise()
{  return mTotNoise;}

float StSsdCluster::getStripMean()
{  return mStripMean;}

int  StSsdCluster::getFlag()
{  return mFlag;}

int StSsdCluster::getIdMcHit(int iR)
{  return mIdMcHit[iR]; }

StSsdCluster* StSsdCluster::getPrevCluster()
{  return mPrevCluster;}

StSsdCluster* StSsdCluster::getNextCluster()
{  return mNextCluster;}

StSsdCluster* StSsdCluster::giveCopy()
{
  StSsdCluster *ptrClone = new StSsdCluster(this->mNCluster);
  ptrClone->mFirstStrip  = this->mFirstStrip;
  ptrClone->mClusterSize = this->mClusterSize;
  ptrClone->mTotAdc      = this->mTotAdc;
  ptrClone->mFirstAdc    = this->mFirstAdc;
  ptrClone->mLastAdc     = this->mLastAdc;
  ptrClone->mTotNoise    = this->mTotNoise;
  ptrClone->mStripMean   = this->mStripMean;
  ptrClone->mFlag        = this->mFlag;
  for(int e=0;e<MAXIDMCHIT;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
  return ptrClone;
}

void StSsdCluster::copyTo(StSsdCluster *ptrClone)
{
  ptrClone->mFirstStrip  = this->mFirstStrip;
  ptrClone->mClusterSize = this->mClusterSize;
  ptrClone->mFlag        = this->mFlag;
  ptrClone->mTotAdc      = this->mTotAdc;
  ptrClone->mFirstAdc    = this->mFirstAdc;
  ptrClone->mLastAdc     = this->mLastAdc;
  ptrClone->mTotNoise    = this->mTotNoise;
  ptrClone->mStripMean   = this->mStripMean;
  for(int e=0;e<MAXIDMCHIT;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
}


void StSsdCluster::update(StSsdStrip *ptr,float rWeight)
{
  int tmpTotAdc = this->mTotAdc;
  this->mTotAdc += (int)(ptr->getDigitSig()*rWeight);
  this->mTotNoise += (int)(ptr->getSigma()*rWeight);
  if(ptr->getNStrip()<this->mFirstStrip) 
    {
      this->mFirstStrip = ptr->getNStrip();
      this->mFirstAdc   = (int)(ptr->getDigitSig()*rWeight);
    }
  if(ptr->getNStrip()>=(this->mFirstStrip+this->mClusterSize))
    {
      this->mLastAdc    = (int)(ptr->getDigitSig()*rWeight);
    }
  this->mStripMean = (tmpTotAdc*this->mStripMean+ptr->getNStrip()*(int)(ptr->getDigitSig()*rWeight))/mTotAdc;


  // for evaluation only...
  int a = 0;
  int b = 0;
  int c = 0;
  int flag = 1;
  while((a<MAXIDMCHIT)&&(this->getIdMcHit(a)!=0)) a++; 
  while((b<MAXIDMCHIT)&&(a<MAXIDMCHIT)&&(ptr->getIdMcHit(b)!=0))
    {
      flag = 1;
      for(c=0;c<MAXIDMCHIT-a;c++)
	{if(this->getIdMcHit(c)==ptr->getIdMcHit(b)) flag=0;}
      if(flag)
	{     
	  this->setIdMcHit(ptr->getIdMcHit(b),a);
	  a++;
	}
      b++;
    }
  this->mClusterSize++;  
  return;
}

