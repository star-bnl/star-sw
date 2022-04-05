//$Id: StSstCluster.cc,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstCluster.cc,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "StSstUtil/StSstCluster.hh"

StSstCluster::StSstCluster(Int_t rNCluster)
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
  mIdMcHit     = new int[SST_MAXIDMCHIT];
  for(Int_t e=0;e<SST_MAXIDMCHIT;e++)  mIdMcHit[e] = 0;
  mPrevCluster = 0;
  mNextCluster = 0;
}

StSstCluster::StSstCluster(Int_t rNCluster, Int_t rFirstStrip, Int_t rClusterSize, Int_t rTotAdc, Int_t rFirstAdc, Int_t rLastAdc, Float_t rTotNoise, Float_t rStripMean, Int_t rFlag, Int_t *rIdMcHit)
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
  mIdMcHit     = new int[SST_MAXIDMCHIT];
  for(Int_t e=0;e<SST_MAXIDMCHIT;e++)  mIdMcHit[e] = rIdMcHit[e];
  mPrevCluster = 0;
  mNextCluster = 0;
}

StSstCluster::StSstCluster(const StSstCluster & originalCluster)
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
  mIdMcHit     = new int[SST_MAXIDMCHIT];
  for(Int_t e=0;e<SST_MAXIDMCHIT;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];
  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
}

StSstCluster::~StSstCluster()
{  delete [] mIdMcHit; }

StSstCluster& StSstCluster::operator=(const StSstCluster originalCluster)
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
  for(Int_t e=0;e<SST_MAXIDMCHIT;e++)  mIdMcHit[e] = originalCluster.mIdMcHit[e];
  mPrevCluster = originalCluster.mPrevCluster;
  mNextCluster = originalCluster.mNextCluster;
  return *this;
}

void  StSstCluster::setNCluster(Int_t rNCluster)
{ mNCluster = rNCluster;}

void  StSstCluster::setFirstStrip(Int_t rFirstStrip)
{ mFirstStrip = rFirstStrip;}

void  StSstCluster::setClusterSize(Int_t rClusterSize)
{ mClusterSize = rClusterSize;}

void  StSstCluster::setTotAdc(Int_t rTotAdc)
{ mTotAdc = rTotAdc;}

void  StSstCluster::setFirstAdc(Int_t rFirstAdc)
{ mFirstAdc = rFirstAdc;}

void  StSstCluster::setLastAdc(Int_t rLastAdc)
{ mLastAdc = rLastAdc;}

void  StSstCluster::setTotNoise(Float_t rTotNoise)
{ mTotNoise = rTotNoise;}

void  StSstCluster::setStripMean(Float_t rStripMean)
{ mStripMean = rStripMean;}

void  StSstCluster::setFlag(Int_t rFlag)
{ mFlag = rFlag;}

void  StSstCluster::setIdMcHit(Int_t rIdMcHit, Int_t iR)
{ mIdMcHit[iR] = rIdMcHit; }

void StSstCluster::setPrevCluster(StSstCluster *rPrevCluster)
{ mPrevCluster = rPrevCluster;}

void StSstCluster::setNextCluster(StSstCluster *rNextCluster)
{ mNextCluster = rNextCluster;}


Int_t StSstCluster::getNCluster()
{  return mNCluster;}

Int_t StSstCluster::getFirstStrip()
{  return mFirstStrip;}

Int_t StSstCluster::getClusterSize()
{  return mClusterSize;}

Int_t StSstCluster::getTotAdc()
{  return mTotAdc;}

Int_t StSstCluster::getFirstAdc()
{  return mFirstAdc;}

Int_t StSstCluster::getLastAdc()
{  return mLastAdc;}

Float_t StSstCluster::getTotNoise()
{  return mTotNoise;}

Float_t StSstCluster::getStripMean()
{  return mStripMean;}

Int_t  StSstCluster::getFlag()
{  return mFlag;}

Int_t StSstCluster::getIdMcHit(Int_t iR)
{  return mIdMcHit[iR]; }

StSstCluster* StSstCluster::getPrevCluster()
{  return mPrevCluster;}

StSstCluster* StSstCluster::getNextCluster()
{  return mNextCluster;}

StSstCluster* StSstCluster::giveCopy()
{
  StSstCluster *ptrClone = new StSstCluster(this->mNCluster);
  ptrClone->mFirstStrip  = this->mFirstStrip;
  ptrClone->mClusterSize = this->mClusterSize;
  ptrClone->mTotAdc      = this->mTotAdc;
  ptrClone->mFirstAdc    = this->mFirstAdc;
  ptrClone->mLastAdc     = this->mLastAdc;
  ptrClone->mTotNoise    = this->mTotNoise;
  ptrClone->mStripMean   = this->mStripMean;
  ptrClone->mFlag        = this->mFlag;
  for(Int_t e=0;e<SST_MAXIDMCHIT;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
  return ptrClone;
}

void StSstCluster::copyTo(StSstCluster *ptrClone)
{
  ptrClone->mFirstStrip  = this->mFirstStrip;
  ptrClone->mClusterSize = this->mClusterSize;
  ptrClone->mFlag        = this->mFlag;
  ptrClone->mTotAdc      = this->mTotAdc;
  ptrClone->mFirstAdc    = this->mFirstAdc;
  ptrClone->mLastAdc     = this->mLastAdc;
  ptrClone->mTotNoise    = this->mTotNoise;
  ptrClone->mStripMean   = this->mStripMean;
  for(Int_t e=0;e<SST_MAXIDMCHIT;e++)  ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
}


void StSstCluster::update(StSstStrip *ptr,Float_t rWeight)
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
  while((a<SST_MAXIDMCHIT)&&(this->getIdMcHit(a)!=0)) a++; 
  while((b<SST_MAXIDMCHIT)&&(a<SST_MAXIDMCHIT)&&(ptr->getIdMcHit(b)!=0))
    {
      flag = 1;
      for(c=0;c<SST_MAXIDMCHIT-a;c++)
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

