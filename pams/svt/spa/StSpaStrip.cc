#include "StSpaStrip.hh"
StSpaStrip::StSpaStrip(int rNStrip, int rMcStrip, int rDigitSig, float rAnalogSig, int *rIdMcHit)
{
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mDigitSig      = rDigitSig;  
  mAnalogSig     = rAnalogSig;
  mIdMcHit       = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = rIdMcHit[e];
  mPrevStrip     = 0;
  mNextStrip     = 0;
}
StSpaStrip::StSpaStrip(int rNStrip, int rMcStrip, float rAnalogSig)
{
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mDigitSig      = 0;  
  mAnalogSig     = rAnalogSig;
  mIdMcHit       = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = 0;
  mPrevStrip     = 0;
  mNextStrip     = 0;
}

StSpaStrip::StSpaStrip(int rNStrip, int rDigitSig)
{
  mNStrip        = rNStrip;
  mMcStrip       = 0;
  mDigitSig      = rDigitSig;  
  mAnalogSig     = 0.;
  mIdMcHit       = new int[5];
  for(int e=0;e<5;e++)  mIdMcHit[e] = 0;
  mPrevStrip     = 0;
  mNextStrip     = 0;
}

StSpaStrip::~StSpaStrip()
{  delete [] mIdMcHit; }

void StSpaStrip::setNStrip(int rNStrip)
{  this->mNStrip = rNStrip; }

void StSpaStrip::setMcStrip(int rMcStrip)
{  this->mMcStrip = rMcStrip; }

void StSpaStrip::setDigitSig(int rDigitSig)
{  this->mDigitSig = rDigitSig; }

void StSpaStrip::setAnalogSig(float rAnalogSig)
{  this->mAnalogSig = rAnalogSig; }

void StSpaStrip::setIdMcHit(int rIdMcHit, int iR)
{  this->mIdMcHit[iR] = rIdMcHit; }

void StSpaStrip::setPrevStrip(StSpaStrip *rPrevStrip)
{  this->mPrevStrip = rPrevStrip; }

void StSpaStrip::setNextStrip(StSpaStrip *rNextStrip)
{  this->mNextStrip = rNextStrip; }

int StSpaStrip::getNStrip()
{  return this->mNStrip; }

int StSpaStrip::getMcStrip()
{  return this->mMcStrip; }

int StSpaStrip::getDigitSig()
{  return this->mDigitSig; }

float StSpaStrip::getAnalogSig()
{  return this->mAnalogSig; }

int StSpaStrip::getIdMcHit(int iR)
{  return this->mIdMcHit[iR]; }

StSpaStrip* StSpaStrip::getPrevStrip()
{  return this->mPrevStrip; }

StSpaStrip* StSpaStrip::getNextStrip()
{  return this->mNextStrip; }


StSpaStrip* StSpaStrip::giveCopy()
{
  StSpaStrip *ptrClone = new StSpaStrip(this->mNStrip, this->mMcStrip, this->mDigitSig, this->mAnalogSig, this->mIdMcHit);
  return ptrClone;
}
