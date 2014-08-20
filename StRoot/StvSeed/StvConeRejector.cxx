#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "TVector3.h"
#include "StvConeRejector.h"
#include "StvSeedConst.h"


enum {kFstAng=88,kMinLen=3,kMaxLen=50,kDivLen=5};
static const double kFstCos = cos(kFstAng*M_PI/180);

//_____________________________________________________________________________
//_____________________________________________________________________________
inline static float Dot(const float A[3],const float B[3])
{
   return A[0]*B[0]+A[1]*B[1]+A[2]*B[2];
}
//_____________________________________________________________________________
inline static void Cop(float A[3],const float B[3])
{   
   A[0]=B[0];A[1]=B[1];A[2]=B[2];
}
//_____________________________________________________________________________
inline static void Mul(float A[3],float F,const float B[3])
{   
   A[0]=B[0]*F; A[1]=B[1]*F; A[2]=B[2]*F;
}
//_____________________________________________________________________________
inline static void Sub(float A[3],const float B[3],const float C[3])
{   
   A[0]=B[0]-C[0]; A[1]=B[1]-C[1]; A[2]=B[2]-C[2];
}
//_____________________________________________________________________________
StvConeRejector::StvConeRejector()
{
  memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
void StvConeRejector::Reset(const float pos[3],const float dir[3]
                           ,float rad,float theta,float err)
///	Definition of start position, direction and radius of cone sector
///	if direction,radius,angle or error are not defined, then
///	they are calculated automatically.

{
  Cop(mPos,pos);
  float nor = sqrt(Dot(mPos,mPos));
  if (rad ) { mOutRad = rad;}	//rad is defined, use it
  else      { 			//rad is no defined, estimate it
    mOutRad = nor/kDivLen;
    if (mOutRad<kMinLen) mOutRad=kMinLen;
    if (mOutRad>kMaxLen) mOutRad=kMaxLen;
  }
  mOutRad2 = mOutRad*mOutRad;
  mRxy2 = mPos[0]*mPos[0]+mPos[1]*mPos[1];
  mRxy  = sqrt(mRxy2);

  if (dir) { 	//Direction defined
    Cop(mDir,dir);
  } else   {	//Estimate dir as direction to 0,0,0
    Mul(mDir,-1./nor,mPos);
  }
  mThet = (theta) ? theta : kFstAng*M_PI/180;
  mSin = sin(mThet);
  mCos = cos(mThet);
  mErr = (err)? err : SEED_ERR(nor);
  mTan2 = pow(mSin/mCos,2);
}

//_____________________________________________________________________________
void StvConeRejector::Prepare()
{
/// 	Calculation of brik sarrounding our cone sector
///	Parameters of brik are used for selection in multy key iterator
///	of hits

  double along = mOutRad*mCos;
  double ortho = mOutRad*mSin;
  for (int i=0;i<3;i++) {
    mLim[0][i] = -mErr;
    mLim[1][i] =  mErr;
    float qwe = along*mDir[i];
    float nor = sqrt(fabs(1-mDir[i]*mDir[i]));
    float asd = ortho*nor;
    float lim = qwe - asd - mErr;
    if (mLim[0][i]>lim)	mLim[0][i] = lim;
    lim = qwe + asd + mErr;
    if (mLim[1][i]<lim)	mLim[1][i] = lim;

		// if spheric part of "cone" is important
    if      (fabs(mDir[i])> mCos*nor) {mLim[1][i] =  mOutRad;} 
    else if (fabs(mDir[i])<-mCos*nor) {mLim[0][i] = -mOutRad;}
      
      		// Move to global system 
    mLim[0][i]+= mPos[i];
    mLim[1][i]+= mPos[i];
  }
}   
//_____________________________________________________________________________
int StvConeRejector::Reject(const float x[3]) const
{
  float xx[3];
  Sub(xx,x,mPos);
  float r2 = xx[0]*xx[0]+xx[1]*xx[1]+xx[2]*xx[2];
  if (r2>mOutRad2)	return 1;
  float myX = Dot(xx,mDir);
  if (myX <0) 	return 2;

  float myY2 = (r2-myX*myX);
  if (myY2 >myX*myX*mTan2) return 3;
  return 0;
}
