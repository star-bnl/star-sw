#include "StSceComp.hh"
StSceComp::StSceComp(int rComp, int rProb, int rGhostOrTrue, int rKindPackage, int rIdMatch, int rIdWaf, float *rD2e, float *rDxg, float *rDxl)
{
  int e            = 0;

  mComp            = rComp;
  mProb            = rProb;
  mGhostOrTrue     = rGhostOrTrue;
  mKindPackage     = rKindPackage;
  mIdMatch         = rIdMatch;
  mIdWaf           = rIdWaf;

  mD2e             = new float[2];
  for (e = 0; e < 2; e++)
    mD2e[e]        = rD2e[e];

  mDxg             = new float[3];
  mDxl             = new float[3];
  for (e = 0; e < 3; e++)
    {
      mDxg[e]      = rDxg[e];
      mDxl[e]      = rDxl[e];
    }

  mPrevComp = 0;
  mNextComp = 0;
}

StSceComp::~StSceComp()
{
  delete [] mD2e;
  delete [] mDxg;
  delete [] mDxl;
}

void StSceComp::setNComp(int rComp)
{  mComp = rComp; }

void StSceComp::setProb(int rProb)
{  mProb = rProb; }

void StSceComp::setGhostOrTrue(int rGhostOrTrue)
{  mGhostOrTrue = rGhostOrTrue; }

void StSceComp::setKindPackage(int rKindPackage)
{  mKindPackage = rKindPackage; }

void StSceComp::setIdMatch(int rIdMatch)
{  mIdMatch = rIdMatch;}

void StSceComp::setIdWaf(int rIdWaf)
{  mIdWaf = rIdWaf; }

void StSceComp::setD2e(float rD2e, int iR)
{  mD2e[iR] = rD2e; }

void StSceComp::setDxg(float rDxg, int iR)
{  mDxg[iR] = rDxg; }

void StSceComp::setDxl(float rDxl, int iR)
{  mDxl[iR] = rDxl; }


void StSceComp::setPrevComp(StSceComp *rPrevComp)
{  mPrevComp = rPrevComp; }

void StSceComp::setNextComp(StSceComp *rNextComp)
{  mNextComp = rNextComp; }


int StSceComp::getNComp()
{  return mComp; }

int StSceComp::getProb()
{  return mProb; }

int StSceComp::getGhostOrTrue()
{  return mGhostOrTrue; }

int StSceComp::getKindPackage()
{  return mKindPackage; }

int StSceComp::getIdMatch()
{  return mIdMatch; }

int StSceComp::getIdWaf()
{  return mIdWaf; }

float StSceComp::getD2e(int iR)
{  return mD2e[iR]; }

float StSceComp::getDxg(int iR)
{  return mDxg[iR]; }

float StSceComp::getDxl(int iR)
{  return mDxl[iR]; }


StSceComp* StSceComp::getPrevComp()
{  return mPrevComp; }

StSceComp* StSceComp::getNextComp()
{  return mNextComp; }


StSceComp* StSceComp::giveCopy()
{
  StSceComp *ptr_clone = new StSceComp(this->mComp, this->mProb, this->mGhostOrTrue, this->mKindPackage, this->mIdMatch, this->mIdWaf, this->mD2e, this->mDxg, this->mDxl);
  int e = 0;
  for (e = 0; e < 2; e++)
      ptr_clone->mD2e[e]     = this->mD2e[e];
  for (e = 0; e < 3; e++)
    {
      ptr_clone->mDxg[e]     = this->mDxg[e];
      ptr_clone->mDxl[e]     = this->mDxl[e];
    }
  return ptr_clone;
}
