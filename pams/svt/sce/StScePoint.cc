#include "StScePoint.hh"

StScePoint::StScePoint(int rPoint, int rIdMcHit, int rIdMcTrack, int rIdWaf,  float *rXg, float rDe, float *rAngle)
{
  int e            = 0;
  mFlag            = 0;
  mPoint           = rPoint;
  mIdCluster       = 0;
  mIdGlobTrk       = 0;
  mIdMatch         = rIdMcHit;

  mIdMcHit         = new int[5];
  mIdMcTrack       = new int[5];
  mIdTrack         = new int[5];

  mIdMcHit[0]      = rIdMcHit;
  mIdMcTrack[0]    = rIdMcTrack;
  mIdTrack[0]      = 0;

  for (e = 1; e < 5; e++)
    {
      mIdMcHit[e]   = 0;
      mIdMcTrack[e] = 0;
      mIdTrack[e]   = 0;
    }
  mIdWaf            = rIdWaf;
  mCov              = new float[3];
  mRes              = new float[3];
  mXg               = new float[3];
  mXl               = new float[3];
  for (e = 0; e < 3; e++)
    {
      mCov[e]       = 0;
      mRes[e]       = 0;
      mXg[e]        = rXg[e];
      mXl[e]        = 0;
    }
  mMom2             = new float[2];
  mDe               = new float[2];

  mMom2[0]      = 0;
  mDe[0]        = rDe;

  for (e = 1; e < 2; e++)
    {
      mMom2[e]      = 0;
      mDe[e]        = 0;
    }

  mUpos      = new float[2];
  mAngle     = new float[2];

  for(e = 0; e < 2; e++)
    {
      mUpos[e]   = 0.;
      mAngle[e]  = rAngle[e];
    }

  mPrevPoint = 0;
  mNextPoint = 0;
}

StScePoint::StScePoint(int rFlag , int rPoint, int rIdCluster, int rIdGlobTrk, int rIdMatch, int *rIdMcHit, int *rIdMcTrack, int *rIdTrack, int rIdWaf, float *rCov , float *rRes, float *rXg, float *rXl, float *rMom2, float *rDe)
{
  int e            = 0;
  mFlag            = rFlag;
  mPoint           = rPoint;
  mIdCluster       = rIdCluster;
  mIdGlobTrk       = rIdGlobTrk;
  mIdMatch         = rIdMatch;

  mIdMcHit         = new int[5];
  mIdMcTrack       = new int[5];
  mIdTrack         = new int[5];
  for (e = 0; e < 5; e++)
    {
      mIdMcHit[e]   = rIdMcHit[e];
      mIdMcTrack[e] = rIdMcTrack[e];
      mIdTrack[e]   = rIdTrack[e];
    }
  mIdWaf            = rIdWaf;
  mCov              = new float[3];
  mRes              = new float[3];
  mXg               = new float[3];
  mXl               = new float[3];
  for (e = 0; e < 3; e++)
    {
      mCov[e]       = rCov[e];
      mRes[e]       = rRes[e];
      mXg[e]        = rXg[e];
      mXl[e]        = rXl[e];
    }
  mMom2             = new float[2];
  mDe               = new float[2];
  for (e = 0; e < 2; e++)
    {
      mMom2[e]      = rMom2[e];
      mDe[e]        = rDe[e];
    }

  mUpos      = new float[2];
  mAngle     = new float[2];

  for(e = 0; e < 2; e++)
    {
      mUpos[e]   = 0.;
      mAngle[e]  = 0;
    }

  mPrevPoint = 0;
  mNextPoint = 0;
}

StScePoint::~StScePoint()
{
  delete [] mIdMcHit;
  delete [] mIdMcTrack;
  delete [] mIdTrack;
  delete [] mCov;
  delete [] mRes;
  delete [] mXg;
  delete [] mXl;
  delete [] mMom2;
  delete [] mDe;
  delete [] mUpos;
  delete [] mAngle;

}

void StScePoint::setFlag(int rFlag)
{  mFlag = rFlag; }

void StScePoint::setNPoint(int rPoint)
{  mPoint = rPoint; }

void StScePoint::setIdCluster(int rIdCluster)
{  mIdCluster = rIdCluster; }

void StScePoint::setIdGlobTrk(int rIdGlobTrk)
{  mIdGlobTrk = rIdGlobTrk; }

void StScePoint::setIdMatch(int rIdMatch)
{  mIdMatch = rIdMatch;}

void StScePoint::setIdMcHit(int rIdMcHit, int iR)
{  mIdMcHit[iR] = rIdMcHit; }

void StScePoint::setIdMcTrack(int rIdMcTrack, int iR)
{  mIdMcTrack[iR] = rIdMcTrack; }

void StScePoint::setIdTrack(int rIdTrack, int iR)
{  mIdTrack[iR] = rIdTrack; }

void StScePoint::setIdWaf(int rIdWaf)
{  mIdWaf = rIdWaf; }

void StScePoint::setCov(float rCov, int iR)
{  mCov[iR] = rCov; }

void StScePoint::setRes(float rRes, int iR)
{  mRes[iR] = rRes; }

void StScePoint::setXg(float rXg, int iR)
{  mXg[iR] = rXg; }

void StScePoint::setXl(float rXl, int iR)
{  mXl[iR] = rXl; }

void StScePoint::setMom2(float rMom2, int iR)
{  mMom2[iR] = rMom2; }

void StScePoint::setDe(float rDe, int iR)
{  mDe[iR] = rDe; }

void StScePoint::setUpos(float rUpos, int iR)
{ mUpos[iR] = rUpos; }

void StScePoint::setAngle(float rAngle, int iR)
{ mAngle[iR] = rAngle; }


void StScePoint::setPrevPoint(StScePoint *rPrevPoint)
{  mPrevPoint = rPrevPoint; }

void StScePoint::setNextPoint(StScePoint *rNextPoint)
{  mNextPoint = rNextPoint; }


int StScePoint::getFlag()
{  return mFlag; }

int StScePoint::getNPoint()
{  return mPoint; }

int StScePoint::getIdCluster()
{  return mIdCluster; }

int StScePoint::getIdGlobTrk()
{  return mIdGlobTrk; }

int StScePoint::getIdMatch()
{  return mIdMatch; }

int StScePoint::getIdMcHit(int iR)
{  return mIdMcHit[iR]; }

int StScePoint::getIdMcTrack(int iR)
{  return mIdMcTrack[iR]; }

int StScePoint::getIdTrack(int iR)
{  return mIdTrack[iR]; }

int StScePoint::getIdWaf()
{  return mIdWaf; }

float StScePoint::getCov(int iR)
{  return mCov[iR]; }

float StScePoint::getRes(int iR)
{  return mRes[iR]; }

float StScePoint::getXg(int iR)
{  return mXg[iR]; }

float StScePoint::getXl(int iR)
{  return mXl[iR]; }

float StScePoint::getMom2(int iR)
{  return mMom2[iR]; }

float StScePoint::getDe(int iR)
{  return mDe[iR]; }

float StScePoint::getUpos(int iR)
{ return mUpos[iR]; }

float StScePoint::getAngle(int iR)
{ return mAngle[iR]; }


StScePoint* StScePoint::getPrevPoint()
{  return mPrevPoint; }

StScePoint* StScePoint::getNextPoint()
{  return mNextPoint; }


StScePoint* StScePoint::giveCopy()
{
  StScePoint *ptr_clone = new StScePoint(this->mFlag , this->mPoint, this->mIdCluster, this->mIdGlobTrk, this->mIdMatch, this->mIdMcHit, this->mIdMcTrack, this->mIdTrack, this->mIdWaf, this->mCov , this->mRes, this->mXg, this->mXl, this->mMom2, this->mDe);
  int e = 0;
  for (e = 0; e < 5; e++)
    {
      ptr_clone->mIdMcHit[e]    = this->mIdMcHit[e];
      ptr_clone->mIdMcTrack[e]  = this->mIdMcTrack[e];
      ptr_clone->mIdTrack[e]    = this->mIdTrack[e];
    }
  for (e = 0; e < 3; e++)
    {
      ptr_clone->mCov[e]    = this->mCov[e];
      ptr_clone->mRes[e]    = this->mRes[e];
      ptr_clone->mXg[e]     = this->mXg[e];
      ptr_clone->mXl[e]     = this->mXl[e];
    }
  for (e = 0; e < 2; e++)
    {
      ptr_clone->mMom2[e]   = this->mMom2[e];
      ptr_clone->mDe[e]     = this->mDe[e];
    }
  return ptr_clone;
}
