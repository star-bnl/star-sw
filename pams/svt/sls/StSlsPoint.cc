#include "StSlsPoint.hh"

StSlsPoint::StSlsPoint(int rNId , int rMcHit , int rMcTrack , float *rXg , float rDe, float *rAngle)
{
  int i      = 0;
  mNId       = rNId;
  mMcHit     = rMcHit;
  mMcTrack   = rMcTrack;
  mXl        = new float[3];
  mXg        = new float[3];
  mUpos      = new float[2];
  mAngle     = new float[2];

  for(i = 0; i < 3; i++)
    {
      mXl[i]     = 0.;
      mXg[i]     = rXg[i];
    }

  for(i = 0; i < 2; i++)
    {
      mUpos[i]   = 0.;
      mAngle[i]  = rAngle[i];
    }

  mDe        = rDe;

  mPrevPoint = 0;
  mNextPoint = 0;
}

StSlsPoint::~StSlsPoint()
{
  delete [] mXl;
  delete [] mXg;
  delete [] mUpos;
  delete [] mAngle;
}

void StSlsPoint::setNId(int rNId)
{ mNId = rNId; }

void StSlsPoint::setMcHit(int rMcHit)
{ mMcHit = rMcHit; }

void StSlsPoint::setMcTrack(int rMcTrack)
{ mMcTrack = rMcTrack; }

void StSlsPoint::setXl(float rXl, int iR)
{ mXl[iR] = rXl; }

void StSlsPoint::setXg(float rXg, int iR)
{ mXg[iR] = rXg; }

void StSlsPoint::setUpos(float rUpos, int iR)
{ mUpos[iR] = rUpos; }

void StSlsPoint::setAngle(float rAngle, int iR)
{ mAngle[iR] = rAngle; }

void StSlsPoint::setDe(float rDe)
{ mDe = rDe; }


void StSlsPoint::setPrevPoint(StSlsPoint *rPrevPoint)
{ mPrevPoint = rPrevPoint; }

void StSlsPoint::setNextPoint(StSlsPoint *rNextPoint)
{ mNextPoint = rNextPoint; }


int StSlsPoint::getNId()
{ return mNId; }

int StSlsPoint::getMcHit()
{ return mMcHit; }

int StSlsPoint::getMcTrack()
{ return mMcTrack; }

float StSlsPoint::getXl(int iR)
{ return mXl[iR]; }

float StSlsPoint::getXg(int iR)
{ return mXg[iR]; }

float StSlsPoint::getUpos(int iR)
{ return mUpos[iR]; }

float StSlsPoint::getAngle(int iR)
{ return mAngle[iR]; }

float StSlsPoint::getDe()
{ return mDe; }


StSlsPoint* StSlsPoint::getPrevPoint()
{ return mPrevPoint; }

StSlsPoint* StSlsPoint::getNextPoint()
{ return mNextPoint; }


StSlsPoint* StSlsPoint::giveCopy()
{
  StSlsPoint *ptr_clone = new StSlsPoint(this->mNId, this->mMcHit, this->mMcTrack, this->mXg, this->mDe, this->mAngle);
  int i = 0;
  for(i = 0; i < 3; i++)
    ptr_clone->mXl[i]    = this->mXl[i];

  for(i = 0; i < 2; i++)
    ptr_clone->mUpos[i]  = this->mUpos[i];

  return ptr_clone;
}
