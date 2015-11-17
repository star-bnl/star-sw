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
  mRxy2 = mPos[0]*mPos[0]+mPos[1]*mPos[1];
  mR2   = mRxy2 + mPos[2]*mPos[2];
  mRxy  = sqrt(mRxy2);
  mErr = SEED_ERR(mRxy);

  if (dir) { 	//Direction defined
    Cop(mDir,dir);
    mThet = (theta) ? theta : kFstAng*M_PI/180;
  } else   {	//Estimate dir as direction to 0,0,0

    double norL = 1./sqrt(mRxy2+pow(pos[2]-kZRange,2)); 
    double norR = 1./sqrt(mRxy2+pow(pos[2]+kZRange,2)); 
    double norQ = (norL+norR);
    mDir[0]= pos[0]*norQ;
    mDir[1]= pos[1]*norQ;
    mDir[2]= pos[2]*norQ +kZRange*(norR-norL);
    norQ = 1./sqrt(mDir[0]*mDir[0]+mDir[1]*mDir[1]+mDir[2]*mDir[2]);
    for (int i=0;i<3;i++) {mDir[i]*=(-norQ);}
    mThet = acos(-(mDir[0]*pos[0]+mDir[1]*pos[1]+mDir[2]*(pos[2]+kZRange))*norR);
    assert(mThet< M_PI/2);
  }
  if (rad ) { mOutRad = rad;}	//rad is defined, use it
  else      { 			//rad is no defined, estimate it
    mOutRad = sqrt(mR2)/kDivLen;
    if (mOutRad<kMinLen) mOutRad=kMinLen;
    if (mOutRad>kMaxLen) mOutRad=kMaxLen;
  }
  mOutRad2 = (mOutRad+mErr)*(mOutRad+mErr);
  mSin = sin(mThet);
  mCos = cos(mThet);
  mTan2 = pow(mSin/mCos,2);
}
//_____________________________________________________________________________
void StvConeRejector::Prepare()
{
/// 	Calculation of brik sarrounding our cone sector
///	Parameters of brik are used for selection in multy key iterator
///	of hits
static const double kSqrHlf = sqrt(0.5);
  double along = mOutRad*mCos;
  double ortho = mOutRad*mSin;
  mPos[3] = ( mPos[0] + mPos[1])*kSqrHlf;
  mPos[4] = (-mPos[0] + mPos[1])*kSqrHlf;
  mDir[3] = ( mDir[0] + mDir[1])*kSqrHlf;
  mDir[4] = (-mDir[0] + mDir[1])*kSqrHlf;

  
  for (int i=0;i<5;i++) {
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
static int mytimes = 20;
  if (mytimes >0) {
    mytimes--;
    double fullVol = M_PI*200*200*400/100;
    double vol1=1/fullVol; for (int i=0;i<3;i++) 	{ vol1*=mLim[1][i]-mLim[0][i];}
    double vol2=1/fullVol; for (int i: {2,3,4}  ) 	{ vol2*=mLim[1][i]-mLim[0][i];}
    printf (" vol1 = %g Vol2 = %g\n",vol1,vol2);
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
