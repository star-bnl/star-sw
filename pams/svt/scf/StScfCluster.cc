#include "StScfCluster.hh"
StScfCluster::StScfCluster(int rNCluster)
{
  mNCluster    = rNCluster;
  mFirstStrip  = 769;
  mClusterSize = 0;
  mFlag        = 0;
  mTotAdc      = 0;
  mFirstAdc    = 0;
  mLastAdc     = 0;
  mTotNoise    = 0;
  mStripMean   = 0;
  mIdMcHit     = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = 0;
  mPrevCluster = 0;
  mNextCluster = 0;
}

StScfCluster::~StScfCluster()
{  delete [] mIdMcHit; }

void StScfCluster::setPrevCluster(StScfCluster *rPrevCluster)
{  this->mPrevCluster = rPrevCluster;}

void StScfCluster::setNextCluster(StScfCluster *rNextCluster)
{  this->mNextCluster = rNextCluster;}

void  StScfCluster::setNCluster(int rNCluster)
{  this->mNCluster = rNCluster;}

void  StScfCluster::setFirstStrip(int rFirstStrip)
{  this->mFirstStrip = rFirstStrip;}

void  StScfCluster::setClusterSize(int rClusterSize)
{  this->mClusterSize = rClusterSize;}

void  StScfCluster::setFlag(int rFlag)
{  this->mFlag = rFlag;}

void  StScfCluster::setTotAdc(int rTotAdc)
{  this->mTotAdc = rTotAdc;}

void  StScfCluster::setFirstAdc(int rFirstAdc)
{  this->mFirstAdc = rFirstAdc;}

void  StScfCluster::setLastAdc(int rLastAdc)
{  this->mLastAdc = rLastAdc;}

void  StScfCluster::setTotNoise(int rTotNoise)
{  this->mTotNoise = rTotNoise;}

void  StScfCluster::setStripMean(float rStripMean)
{  this->mStripMean = rStripMean;}

void  StScfCluster::setIdMcHit(int rIdMcHit, int iR)
{  this->mIdMcHit[iR] = rIdMcHit; }


int StScfCluster::getNCluster()
{  return this->mNCluster;}

int StScfCluster::getFirstStrip()
{  return this->mFirstStrip;}

int StScfCluster::getClusterSize()
{  return this->mClusterSize;}

int  StScfCluster::getFlag()
{  return this->mFlag;}

int StScfCluster::getTotAdc()
{  return this->mTotAdc;}

int StScfCluster::getFirstAdc()
{  return this->mFirstAdc;}

int StScfCluster::getLastAdc()
{  return this->mLastAdc;}

int StScfCluster::getTotNoise()
{  return this->mTotNoise;}

float StScfCluster::getStripMean()
{  return this->mStripMean;}

int StScfCluster::getIdMcHit(int iR)
{  return this->mIdMcHit[iR]; }

StScfCluster* StScfCluster::getPrevCluster()
{  return this->mPrevCluster;}

StScfCluster* StScfCluster::getNextCluster()
{  return this->mNextCluster;}


void StScfCluster::copyTo(StScfCluster *ptrClone)
{
  ptrClone->mFirstStrip  = this->mFirstStrip;
  ptrClone->mClusterSize = this->mClusterSize;
  ptrClone->mFlag        = this->mFlag;
  ptrClone->mTotAdc      = this->mTotAdc;
  ptrClone->mFirstAdc    = this->mFirstAdc;
  ptrClone->mLastAdc     = this->mLastAdc;
  ptrClone->mTotNoise    = this->mTotNoise;
  ptrClone->mStripMean   = this->mStripMean;
  for(int e=0;e<5;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
}


void StScfCluster::update(StScfStrip *ptr,float rWeight)
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
  while((a<5)&&(this->getIdMcHit(a)!=0)) a++; 
  while((b<5)&&(a<5)&&(ptr->getIdMcHit(b)!=0))
    {
      flag = 1;
      for(c=0;c<5-a;c++)
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

