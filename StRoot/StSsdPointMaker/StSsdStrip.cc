#include "StSsdStrip.hh"

StSsdStrip::StSsdStrip(int rNStrip, int rDigitSig, int rSigma, int *rIdMcHit)
{
  mIdMcHit   = new int[MAXIDMCHIT];

  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;
  mSigma     = rSigma;
  for(int e=0;e<MAXIDMCHIT;e++) mIdMcHit[e] = rIdMcHit[e];
  mPrevStrip = 0;
  mNextStrip = 0;
}

StSsdStrip::StSsdStrip(int rNStrip, int rDigitSig, int rSigma)
{
  mIdMcHit   = new int[MAXIDMCHIT];

  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;  
  mSigma     = rSigma;
  for(int e=0;e<MAXIDMCHIT;e++) mIdMcHit[e] = 0;
  mPrevStrip = 0;
  mNextStrip = 0;
}

StSsdStrip::~StSsdStrip()
{
  delete [] mIdMcHit;
}

void StSsdStrip::setPrevStrip(StSsdStrip *rPrevStrip)
{  this->mPrevStrip = rPrevStrip; }

void StSsdStrip::setNextStrip(StSsdStrip *rNextStrip)
{  this->mNextStrip = rNextStrip; }

void StSsdStrip::setSigma(int rSigma)
{  this->mSigma = rSigma; }

void StSsdStrip::setNStrip(int rNStrip)
{  this->mNStrip = rNStrip; }

void StSsdStrip::setDigitSig(int rDigitSig)
{  this->mDigitSig = rDigitSig; }

void StSsdStrip::setIdMcHit(int rIdMcHit, int iR)
{  this->mIdMcHit[iR] = rIdMcHit; }

int StSsdStrip::getNStrip()
{  return this->mNStrip; }

int StSsdStrip::getDigitSig()
{  return this->mDigitSig; }

StSsdStrip* StSsdStrip::getPrevStrip()
{  return this->mPrevStrip; }

StSsdStrip* StSsdStrip::getNextStrip()
{  return this->mNextStrip; }

int StSsdStrip::getSigma()
{  return this->mSigma; }

int StSsdStrip::getIdMcHit(int iR)
{  return this->mIdMcHit[iR]; }

void StSsdStrip::copyTo(StSsdStrip *ptrClone)
{
  ptrClone->mNStrip    = this->mNStrip;
  ptrClone->mDigitSig  = this->mDigitSig;
  ptrClone->mSigma     = this->mSigma;
  for(int e=0;e<MAXIDMCHIT;e++) ptrClone->mIdMcHit[e] = this->mIdMcHit[e];
}


