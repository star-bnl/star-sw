#include "StSpaNoise.hh"
StSpaNoise::StSpaNoise(int rNStrip, int rPedestal, int rSigma)
{
  mNStrip      = rNStrip;
  mPedestal    = rPedestal;  
  mSigma       = rSigma;
  mNoiseValue  = 0;
  mPrevNoise   = 0;
  mNextNoise   = 0;
  mIsActive    = 1;
}
StSpaNoise::~StSpaNoise()
{
}

void StSpaNoise::setNStrip(int rNStrip)
{ this->mNStrip = rNStrip; }

void StSpaNoise::setPedestal(int rPedestal)
{ this->mPedestal = rPedestal; }

void StSpaNoise::setSigma(int rSigma)
{  this->mSigma = rSigma; }

void StSpaNoise::setNoiseValue(int rNoiseValue)
{ this->mNoiseValue = rNoiseValue; }

void StSpaNoise::setIsActive(int rIsActive)
{ this->mIsActive = rIsActive; }

void StSpaNoise::setPrevNoise(StSpaNoise *rPrevNoise)
{ this->mPrevNoise = rPrevNoise; }

void StSpaNoise::setNextNoise(StSpaNoise *rNextNoise)
{ this->mNextNoise = rNextNoise; }

int StSpaNoise::getNStrip()
{ return this->mNStrip; }

int StSpaNoise::getPedestal()
{ return this->mPedestal; }

int StSpaNoise::getSigma()
{ return this->mSigma; }

int StSpaNoise::getNoiseValue()
{ return this->mNoiseValue; }

int StSpaNoise::getIsActive()
{ return this->mIsActive; }

StSpaNoise* StSpaNoise::getPrevNoise()
{ return this->mPrevNoise; }

StSpaNoise* StSpaNoise::getNextNoise()
{ return this->mNextNoise; }


StSpaNoise* StSpaNoise::giveCopy()
{
  StSpaNoise *ptrClone  = new StSpaNoise(this->mNStrip, this->mPedestal, this->mSigma);
  
  ptrClone->mNoiseValue = this->mNoiseValue;
  ptrClone->mIsActive   = this->mIsActive;
  return ptrClone;
}

