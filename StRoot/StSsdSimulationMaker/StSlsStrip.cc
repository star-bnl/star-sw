#include "StSlsStrip.hh"

StSlsStrip::StSlsStrip(int rNStrip, int rIdHit, int rMcHit, int rMcTrack, float rAnalogSig)
{
  mNStrip        = rNStrip;
  mIdHit         = new int[5];
  mIdMcHit       = new int[5];
  mIdMcTrack     = new int[5];
  mIdHit[0]      = rIdHit;
  mIdMcHit[0]    = rMcHit;
  mIdMcTrack[0]  = rMcTrack;
  for (int i = 1; i < 5; i++)
    {
      mIdHit[i]     = 0;
      mIdMcHit[i]   = 0;
      mIdMcTrack[i] = 0;
    }
  mDigitSig      = 0;  
  mNHits         = 1;
  mAnalogSig     = rAnalogSig;
  mPrevStrip = 0;
  mNextStrip = 0;
}

StSlsStrip::~StSlsStrip()
{
  delete [] mIdHit;
  delete [] mIdMcHit;
  delete [] mIdMcTrack;
}

void StSlsStrip::setNStrip(int rNStrip)
{ mNStrip = rNStrip; }

void StSlsStrip::setIdHit(int rIdHit, int iR)
{ mIdHit[iR] = rIdHit; }

void StSlsStrip::setIdMcHit(int rIdMcHit, int iR)
{ mIdMcHit[iR] = rIdMcHit; }

void StSlsStrip::setIdMcTrack(int rIdMcTrack, int iR)
{ mIdMcTrack[iR] = rIdMcTrack; }

void StSlsStrip::setDigitSig(int rDigitSig)
{ mDigitSig = rDigitSig; }

void StSlsStrip::setNHits(int rNHits)
{ mNHits = rNHits; }

void StSlsStrip::setAnalogSig(float rAnalogSig)
{ mAnalogSig = rAnalogSig; }


void StSlsStrip::setPrevStrip(StSlsStrip *rPrevStrip)
{ mPrevStrip = rPrevStrip; }

void StSlsStrip::setNextStrip(StSlsStrip *rNextStrip)
{ mNextStrip = rNextStrip; }


int StSlsStrip::getNStrip()
{ return mNStrip; }

int StSlsStrip::getIdHit(int iR)
{ return mIdHit[iR]; }

int StSlsStrip::getIdMcHit(int iR)
{ return mIdMcHit[iR]; }

int StSlsStrip::getIdMcTrack(int iR)
{ return mIdMcTrack[iR]; }

int StSlsStrip::getDigitSig()
{ return mDigitSig; }

int StSlsStrip::getNHits()
{ return mNHits; }

float StSlsStrip::getAnalogSig()
{ return mAnalogSig; }


StSlsStrip* StSlsStrip::getPrevStrip()
{ return mPrevStrip; }

StSlsStrip* StSlsStrip::getNextStrip()
{ return mNextStrip; }


void StSlsStrip::copyTo(StSlsStrip *ptrClone)
{
  ptrClone->mNStrip = this->mNStrip;
  for (int i = 0; i < 5; i++)
    {
      ptrClone->mIdHit[i]     = this->mIdHit[i];
      ptrClone->mIdMcHit[i]   = this->mIdMcHit[i];
      ptrClone->mIdMcTrack[i] = this->mIdMcTrack[i];
    }
  ptrClone->mNHits            = this->mNHits;
  ptrClone->mDigitSig         = this->mDigitSig;
  ptrClone->mAnalogSig        = this->mAnalogSig;
}


void StSlsStrip::print()
{
  cout <<  "Strip number " << mNStrip << endl;
  cout <<  "Id  Hit " << mIdHit[0] <<"-"<< mIdHit[1] <<"-"<<mIdHit[2]  <<"-"<< mIdHit[3] <<"-"<< mIdHit[4] << endl ;
  cout <<  "Id Monte-Carlo  Hit " << mIdMcHit[0] <<"-"<< mIdMcHit[1] <<"-"<<mIdMcHit[2]  <<"-"<< mIdMcHit[3] <<"-"<< mIdMcHit[4] << endl ;
  cout <<  "Id Monte_carlo Track" << mIdMcTrack[0] <<"-"<< mIdMcTrack[1] <<"-"<< mIdMcTrack[2]  <<"-"<< mIdMcTrack[3] <<"-"<< mIdMcTrack[4] << endl ;
  cout << "Digital signal on strip " << mDigitSig << endl;
  cout << "Analog signal on strip " << mAnalogSig << endl;
  cout << "Hit(s) associated" << mNHits  << endl;
  if (mPrevStrip) cout << "Previous strip Id" <<   mPrevStrip->getNStrip() << endl;
  if (mNextStrip) cout << "Next strip Id " <<  mNextStrip->getNStrip() << endl ;
}
