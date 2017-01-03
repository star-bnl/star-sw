#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "TVector3.h"
#include "StvConeRejector.h"
#include "StvSeedConst.h"


//VP ??? enum {kFstAng=88,kMinLen=3,kMaxLen=50,kDivLen=5};
enum {kFstAng=88,kMinLen=3,kMaxLen=50,kDivLen=1};
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
inline static void Add(float A[3],const float B[3],const float C[3])
{   
   A[0]=B[0]+C[0]; A[1]=B[1]+C[1]; A[2]=B[2]+C[2];
}
//_____________________________________________________________________________
inline static void Cro(float A[3],const float B[3],const float C[3])
{   
// TVector3(fY*p.fZ-p.fY*fZ, fZ*p.fX-p.fZ*fX, fX*p.fY-p.fX*fY);
   A[0] = B[1]*C[2]-C[1]*B[2];
   A[1] = B[2]*C[0]-C[2]*B[0];
   A[2] = B[0]*C[1]-C[0]*B[1];

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
  double norL=0,norR=0;

  if (dir) { 	//Direction defined
    Cop(mDir[0],dir);
    mThet = (theta) ? theta : kFstAng*M_PI/180;
  } else   {	//Estimate dir as median direction to (0,0,-kZRange) and (0,0,kZRange)

    norL = sqrt(mRxy2+pow(mPos[2]-kZRange,2)); 
    norR = sqrt(mRxy2+pow(mPos[2]+kZRange,2)); 
    double norQ = (1./norL+1./norR);

    mDir[0][0]= mPos[0]*norQ;
    mDir[0][1]= mPos[1]*norQ;
    mDir[0][2]= mPos[2]*norQ +kZRange*(1./norR-1./norL);
    norQ = 1./sqrt(Dot(mDir[0],mDir[0]));
    Mul(mDir[0],-norQ,mDir[0]);
    mThet = (mR2 -(kZRange*kZRange))/(norL*norR);
    mThet = 0.5*acos(mThet);

    assert(mThet< M_PI/2);
  }
  if (rad ) { mOutRad = rad;}	//rad is defined, use it
  else      { 			//rad is no defined, estimate it
    mOutRad = (norL>norR) ? norL:norR;
    mOutRad/= kDivLen;
    if (mOutRad<kMinLen) mOutRad=kMinLen;
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
  mDir[1][0]= -mDir[0][1];
  mDir[1][1]=  mDir[0][0];
  mDir[1][2]= 0;
  double qwe = sqrt(Dot(mDir[1],mDir[1]));
  if (qwe<1e-11) { //Strictly along Z?
    mDir[1][0]=1; mDir[1][1]=0; mDir[1][2]=0;
  } else {
    Mul(mDir[1],1/qwe,mDir[1]);
  }
  Cro(mDir[2],mDir[0],mDir[1]);

  
  for (int i=0;i<3;i++) {
    mLim[0][i] = -mErr;
    mLim[1][i] =  mErr;
    float qwe = along*mDir[0][i];			//Shift to the base of cone
//	Norm sqrt(Bx*Bx+Cx*Cx)==sqrt(1-Ax*Ax)
    float nor = sqrt(fabs(1-mDir[0][i]*mDir[0][i])); 	//Norm 
    float asd = ortho*nor;
    float lim = qwe - asd - mErr;
    if (mLim[0][i]>lim)	mLim[0][i] = lim;
    lim = qwe + asd + mErr;
    if (mLim[1][i]<lim)	mLim[1][i] = lim;

		// if spheric part of "cone" is important
    if      (mDir[0][i]> mCos) {mLim[1][i] =  mOutRad;} 
    else if (mDir[0][i]<-mCos) {mLim[0][i] = -mOutRad;}
      
      		// Move to global system 
    mLim[0][i]+= mPos[i];
    mLim[1][i]+= mPos[i];
  }
static int mytimes = 20;
  if (mytimes >0) {
    TestIt();
    mytimes--;
    double fullVol = M_PI*200*200*400/100;
    double vol1=1/fullVol; for (int i=0;i<3;i++) 	{ vol1*=mLim[1][i]-mLim[0][i];}
    printf (" vol1 = %g \n",vol1);
  }
}   
//_____________________________________________________________________________
int StvConeRejector::Reject(const float x[3]) const
{
  float xx[3];
  Sub(xx,x,mPos);
  float r2 = xx[0]*xx[0]+xx[1]*xx[1]+xx[2]*xx[2];
  if (r2>mOutRad2)		return 1;
  float myX = Dot(xx,mDir[0]);
  if (myX <0) 			return 2;

  float myY2 = (r2-myX*myX);
  if (myY2 >myX*myX*mTan2) 	return 3;
  return 0;
}
#include "TRandom.h"
//_____________________________________________________________________________
int StvConeRejector::TestIt() const
{
  float x[3];
  
  for (int i=0;i<20;i++) {
  
    int ifail = 0;
    double r = (mOutRad*1.5)*gRandom->Rndm();
    if (r >mOutRad) ifail+=1;
    double sThet = mSin*1.1; if(sThet>=0.99) sThet=0.99;
    sThet = sThet*gRandom->Rndm();
    if (sThet>mSin) ifail+=2;
    double cThet = sqrt(1.-sThet*sThet);
    double alfa = M_PI*2*gRandom->Rndm();
    for (int j = 0;j<3;j++) {
      x[j] = r*(mDir[0][j]*cThet + sThet*(mDir[1][j]*cos(alfa)+mDir[2][j]*sin(alfa)));
    }
    if (Dot(x,x)>mOutRad*mOutRad)  ifail+=4;
    Add(x,x,mPos);
    int irej = Reject(x);
    assert((!!irej) == (!!ifail));
   }    
   return 0;
} 
