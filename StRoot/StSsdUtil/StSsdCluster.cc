// $Id: StSsdCluster.cc,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdCluster.cc,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.5  2005/04/25 14:13:23  bouchet
// new method makeScfCtrlHistograms and makeScmCtrlHistograms and Clusternoise is coded as a float
//
// Revision 1.4  2005/03/18 14:24:21  lmartin
// missing CVS header added
//

#include "StSsdUtil/StSsdCluster.hh"

StSsdCluster::StSsdCluster(Int_t rNCluster)
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
  mIdMcHit     = new int[SSD_MAXIDMCHIT];
  for(Int_t e=0;e<SSD_MAXIDMCHIT;e++)  mIdMcHit[e] = 0;
  mPrevCluster = 0;
  mNextCluster = 0;
}

StSsdCluster::StSsdCluster(Int_t rNCluster, Int_t rFirstStrip, Int_t rClusterSize, Int_t rTotAdc, Int_t rFirstAdc, Int_t rLastAdc, Float_t rTotNoise, Float_t rStripMean, Int_t rFlag, Int_t *rIdMcHit)
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
  mIdMcHit     = new int[SSD_MAXIDMCHIT];
  for(Int_t e=0;e<SSD_MAXIDMCHIT;e++)  mIdMcHit[e] = rIdMcHit[e];
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
  mIdMcHit     = new int[SSD_MAXIDMCHIT];
  for(Int_t e=0;e<SSD_MAXIDMCHIT;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];
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
  for(Int_t e=0;e<SSD_MAXIDMCHIT;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];
  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
  return *this;
}

void  StSsdCluster::setNCluster(Int_t rNCluster)
{ mNCluster = rNCluster;}

void  StSsdCluster::setFirstStrip(Int_t rFirstStrip)
{ mFirstStrip = rFirstStrip;}

void  StSsdCluster::setClusterSize(Int_t rClusterSize)
{ mClusterSize = rClusterSize;}

void  StSsdCluster::setTotAdc(Int_t rTotAdc)
{ mTotAdc = rTotAdc;}

void  StSsdCluster::setFirstAdc(Int_t rFirstAdc)
{ mFirstAdc = rFirstAdc;}

void  StSsdCluster::setLastAdc(Int_t rLastAdc)
{ mLastAdc = rLastAdc;}

void  StSsdCluster::setTotNoise(Float_t rTotNoise)
{ mTotNoise = rTotNoise;}

void  StSsdCluster::setStripMean(Float_t rStripMean)
{ mStripMean = rStripMean;}

void  StSsdCluster::setFlag(Int_t rFlag)
{ mFlag = rFlag;}

void  StSsdCluster::setIdMcHit(Int_t rIdMcHit, Int_t iR)
{ mIdMcHit[iR] = rIdMcHit; }

void StSsdCluster::setPrevCluster(StSsdCluster *rPrevCluster)
{ mPrevCluster = rPrevCluster;}

void StSsdCluster::setNextCluster(StSsdCluster *rNextCluster)
{ mNextCluster = rNextCluster;}


Int_t StSsdCluster::getNCluster()
{  return mNCluster;}

Int_t StSsdCluster::getFirstStrip()
{  return mFirstStrip;}

Int_t StSsdCluster::getClusterSize()
{  return mClusterSize;}

Int_t StSsdCluster::getTotAdc()
{  return mTotAdc;}

Int_t StSsdCluster::getFirstAdc()
{  return mFirstAdc;}

Int_t StSsdCluster::getLastAdc()
{  return mLastAdc;}

Float_t StSsdCluster::getTotNoise()
{  return mTotNoise;}

Float_t StSsdCluster::getStripMean()
{  return mStripMean;}

Int_t  StSsdCluster::getFlag()
{  return mFlag;}

Int_t StSsdCluster::getIdMcHit(Int_t iR)
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
  for(Int_t e=0;e<SSD_MAXIDMCHIT;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
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
  for(Int_t e=0;e<SSD_MAXIDMCHIT;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
}


void StSsdCluster::update(StSsdStrip *ptr,Float_t rWeight)
{
  Int_t tmpTotAdc = this->mTotAdc;
  this->mTotAdc += (int)(ptr->getDigitSig()*rWeight);
  this->mTotNoise += (ptr->getSigma()*rWeight);
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
  Int_t a = 0;
  Int_t b = 0;
  Int_t c = 0;
  Int_t flag = 1;
  while((a<SSD_MAXIDMCHIT)&&(this->getIdMcHit(a)!=0)) a++; 
  while((b<SSD_MAXIDMCHIT)&&(a<SSD_MAXIDMCHIT)&&(ptr->getIdMcHit(b)!=0))
    {
      flag = 1;
      for(c=0;c<SSD_MAXIDMCHIT-a;c++)
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

