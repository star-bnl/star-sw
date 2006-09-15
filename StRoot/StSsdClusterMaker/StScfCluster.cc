// $Id: StScfCluster.cc,v 1.5 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScfCluster.cc,v $
// Revision 1.5  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.4  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:33  lmartin
// CVS tags added
//
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

void  StScfCluster::setTotNoise(float rTotNoise)
{  this->mTotNoise = rTotNoise;}

void  StScfCluster::setStripMean(float rStripMean)
{  this->mStripMean = rStripMean;}

void  StScfCluster::setIdMcHit(int rIdMcHit, int iR)
{  this->mIdMcHit[iR] = rIdMcHit; }

void  StScfCluster::setIdMcTrack(int rIdMcTrack, int iR)
{  this->mIdMcTrack[iR] = rIdMcTrack; }

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

float StScfCluster::getTotNoise()
{  return this->mTotNoise;}

float StScfCluster::getStripMean()
{  return this->mStripMean;}

int StScfCluster::getIdMcHit(int iR)
{  return this->mIdMcHit[iR]; }

int StScfCluster::getIdMcTrack(int iR)
{  return this->mIdMcTrack[iR]; }


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


void StScfCluster::update(StScfStrip *ptr,float rWeight,int iSide)
{
  int tmpTotAdc = this->mTotAdc;
  this->mTotAdc += (int)(ptr->getDigitSig()*rWeight);
  this->mTotNoise +=(ptr->getSigma()*rWeight);
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
      for(c=0;c<5;c++)
	{if(this->getIdMcHit(c)==ptr->getIdMcHit(b)) flag=0;
      }
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

