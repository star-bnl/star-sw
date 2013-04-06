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
  if (rad ) { mLen = rad;}	//rad is defined, use it
  else      { 			//rad is no defined, estimate it
    mLen = nor/kDivLen;
    if (mLen<kMinLen) mLen=kMinLen;
    if (mLen>kMaxLen) mLen=kMaxLen;
  }
  mLen2 = mLen*mLen;
  mRad2 = mPos[0]*mPos[0]+mPos[1]*mPos[1];
  mRad  = sqrt(mRad2);

  if (dir) { 	//Direction defined
    Cop(mDir,dir);
  } else   {	//Estimate dir as direction to 0,0,0
    Mul(mDir,-1./nor,mPos);
  }
  mThet = (theta) ? theta : kFstAng*M_PI/180;
  mTan = tan(mThet);
  mErr = (err)? err : SEED_ERR(nor);
}

//_____________________________________________________________________________
void StvConeRejector::Prepare()
{
/// 	Calculation of brik sarrounding our cone sector
///	Parameters of brik are used for selection in multy key iterator
///	of hits

  for (int i=0;i<3;i++) {
    float qwe = mLen*mDir[i];
    float asd = mLen*mTan*sqrt(fabs(1-mDir[i]*mDir[i]));
    float lim = qwe - asd - mErr;
    mLim[0][i] = (lim<0)? lim:-mErr;
    lim = qwe + asd + mErr;
    mLim[1][i] = (lim>0)? lim: mErr;
//		Move to global system 
    mLim[0][i]+= mPos[i];
    mLim[1][i]+= mPos[i];
  }
  for (int i=0;i<2;i++) {
    if (mLim[0][i]< -mRad) mLim[0][i]=-mRad;
    if (mLim[1][i]>  mRad) mLim[1][i]= mRad;
  }
}   
//_____________________________________________________________________________
int StvConeRejector::Reject(const float x[3]) const
{
  float xx[3];
  if (x[0]*x[0]+x[1]*x[1]>mRad2 + 2*mRad*mErr) return 4;
  Sub(xx,x,mPos);
  float r2 = xx[0]*xx[0]+xx[1]*xx[1]+xx[2]*xx[2];
  if (r2>mLen2)	return 1;
  float myX = Dot(xx,mDir);
  if (myX <0) 	return 2;
  if (myX >mLen)return 2;


  float myY = (r2-myX*myX);
  myY = (myY>0)? sqrt(myY):0;
  if (myY >myX*mTan+mErr) return 3;
  return 0;
}
