#include "StScfStrip.hh"
StScfStrip::StScfStrip(int rNStrip, int rDigitSig, int rSigma, int *rIdMcHit)
{
  mIdMcHit   = new int[5];

  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;
  mSigma     = rSigma;
  for(int e=0;e<5;e++) mIdMcHit[e] = rIdMcHit[e];
  mPrevStrip = 0;
  mNextStrip = 0;
}

StScfStrip::StScfStrip(int rNStrip, int rDigitSig, int rSigma)
{
  mIdMcHit   = new int[5];

  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;  
  mSigma     = rSigma;
  for(int e=0;e<5;e++) mIdMcHit[e] = 0;
  mPrevStrip = 0;
  mNextStrip = 0;
}

StScfStrip::~StScfStrip()
{
  delete [] mIdMcHit;
}

void StScfStrip::setPrevStrip(StScfStrip *rPrevStrip)
{  this->mPrevStrip = rPrevStrip; }

void StScfStrip::setNextStrip(StScfStrip *rNextStrip)
{  this->mNextStrip = rNextStrip; }

void StScfStrip::setSigma(int rSigma)
{  this->mSigma = rSigma; }

void StScfStrip::setNStrip(int rNStrip)
{  this->mNStrip = rNStrip; }

void StScfStrip::setDigitSig(int rDigitSig)
{  this->mDigitSig = rDigitSig; }

void StScfStrip::setIdMcHit(int rIdMcHit, int iR)
{  this->mIdMcHit[iR] = rIdMcHit; }

int StScfStrip::getNStrip()
{  return this->mNStrip; }

int StScfStrip::getDigitSig()
{  return this->mDigitSig; }

StScfStrip* StScfStrip::getPrevStrip()
{  return this->mPrevStrip; }

StScfStrip* StScfStrip::getNextStrip()
{  return this->mNextStrip; }

int StScfStrip::getSigma()
{  return this->mSigma; }

int StScfStrip::getIdMcHit(int iR)
{  return this->mIdMcHit[iR]; }

void StScfStrip::copyTo(StScfStrip *ptrClone)
{
  ptrClone->mNStrip    = this->mNStrip;
  ptrClone->mDigitSig  = this->mDigitSig;
  ptrClone->mSigma     = this->mSigma;
  for(int e=0;e<5;e++) ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
}


