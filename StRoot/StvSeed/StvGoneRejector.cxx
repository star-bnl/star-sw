#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "TVector3.h"
#include "StvGoneRejector.h"
#include "StvSeedConst.h"


enum {kFstAng=88,kMinLen=3,kMaxLen=50,kDivLen=5};
static const double kFstCos = cos(kFstAng*M_PI/180);
static const double kFstSin = cos(kFstAng*M_PI/180);

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
inline static void Set(float A[3],float s,int n)
{   
   for (int j=0;j<n;j++) {A[j]=s;};
}
//_____________________________________________________________________________
StvGoneRejector::StvGoneRejector()
{
  memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
void StvGoneRejector::Reset(const float pos[3],const float dir[3]
                           ,float rad,float theta)
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

  mErr = SEED_ERR(mRxy);

  if (dir) { 	//Direction defined
    Cop(mDir,dir);
  } else   {	//Estimate dir as direction to 0,0,0
    Mul(mDir,-1./nor,mPos);
  }
  mThet = (theta) ? theta : kFstAng*M_PI/180;
  mSin = sin(mThet);
  mCos = cos(mThet);
  mTan2 = pow(mSin/mCos,2);
}

//_____________________________________________________________________________
void StvGoneRejector::Prepare()
{
/// 	Calculation of poli Gone sarrounding our track

// 	mPoint array of points in track coordinate, Z=along the track
//  	[0] start track, [1] end of track, [>1] points around


  memset(mPoint[0],0,sizeof(mPoint));
  mPoint[1][2] = mOutRad;
  float dZ = mOutRad*mCos;
  float dR = mOutRad*mSin;
  for (int i=2;i<6;i++) {mPoint[i][2]=dZ;}
  mPoint[2][0] =  dR;
  mPoint[3][1] =  dR;
  mPoint[4][0] = -dR;
  mPoint[5][1] = -dR;

  memset(mPlane[0],0,sizeof(mPlane));
  mPlane[0][2]=-1; // mPlane[0][3]=0;
  mPlane[1][2]= 1;    mPlane[1][3]=mOutRad;

  TVector3 Vi(0,0,mOutRad/2);
  int ipl = 1;
  for (int iside=0;iside<2;iside++) { //1st and end poin of track
    TVector3 V0(mPoint[iside]);
    for (int i1pnt=2;i1pnt<6;i1pnt++) {
      int i2pnt = (i1pnt+1); if (i2pnt>=6) i2pnt = 2;
      TVector3 V1(mPoint[i1pnt]); V1-=V0;
      TVector3 V2(mPoint[i2pnt]); V2-=V0;
      TVector3 Vn = V1.Cross(V2);
      Vn = Vn.Unit();
assert(Vn.Mag2()>=0.999);
      ipl++;
      float *pl = mPlane[ipl];
      pl[3] = Vn.Dot(V0);
      if (Vn.Dot(Vi)-pl[3]>0) 	{ Vn*=-1.; pl[3]*=-1.;}
      for (int j=0;j<3;j++)	{ pl[j] = Vn[j];}
      assert(Vn.Dot(Vi)-pl[3]<=0);

    }
  }
{
   for (UInt_t ip=0;ip<sizeof(mPoint)/(sizeof(mPoint[0][0])*3);ip++) {
     assert(!Reject(mPoint[ip]));
   }  
}
  double sinL = mDir[2];
  double cosL = (1-sinL)*(1+sinL);
  if (cosL <0) 		{cosL = 0;}
  else if (cosL >1) 	{cosL = 1;}
  else 			{cosL = sqrt(cosL);}

  double cosP = 1,sinP=0;;
  if (cosL>1e-5) { cosP=mDir[0]/cosL; sinP=mDir[1]/cosL;}

// Convert points into global sys
  for (int ip=0;ip<6;ip++) {
    float *p = mPoint[ip];

    float zz = p[2];
    p[2] = sinL*zz - cosL*p[0];
    p[0] = cosL*zz + sinL*p[0];

    float xx = p[0];  
    p[0] = cosP*xx - sinP*p[1];
    p[1] = sinP*xx + cosP*p[1];

    Add(p,mPos,p);
  }
  



// Convert planes into global sys
  for (int ip=0;ip<10;ip++) {
    float *p = mPlane[ip];

    float zz = p[2];
    p[2] = sinL*zz - cosL*p[0];
    p[0] = cosL*zz + sinL*p[0];

    float xx = p[0];  
    p[0] = cosP*xx - sinP*p[1];
    p[1] = sinP*xx + cosP*p[1];

    p[3] +=Dot(p,mPos);
    if (ip) continue;
//		Test first plane N == mDir
    assert(fabs(p[0]+mDir[0])+fabs(p[1]+mDir[1])+fabs(p[2]+mDir[2])<1e-5);

  }


//	Create limits
// Convert points into global sys
  Set(mLim[0], 1e11,6);
  Set(mLim[1],-1e11,6);
  
  for (int ip=0;ip<6;ip++) {
    const float *p = mPoint[ip];
    for (int j=0;j<3;j++) {
      if (mLim[0][j]>p[j]) mLim[0][j]=p[j];
      if (mLim[1][j]<p[j]) mLim[1][j]=p[j];
    }
      
    for (int j=3;j<6;j++) {
      int jj = (j<5)? j+1:3;
      float s = p[j]-p[jj];
      if (mLim[0][j]>s) mLim[0][j]=s;
      if (mLim[1][j]<s) mLim[1][j]=s;
    }
  }
//		Test it
{
   float myX[3];
   for (int j=0;j<3;j++) {myX[j]=mPos[j]+mDir[j]*mOutRad*0.5;}
   assert(!Reject(myX));
   for (UInt_t ip=0;ip<sizeof(mPoint)/(sizeof(mPoint[0][0])*3);ip++) {
     assert(!Reject(mPoint[ip]));
   }  



}


}   
//_____________________________________________________________________________
int StvGoneRejector::Reject(const float x[3]) const
{
  for (int ipl=0;ipl<10;ipl++) {
    const float *p = mPlane[ipl];
    if (Dot(p,x)-p[3]>mErr) return 77;
  }
  return 0;
}
