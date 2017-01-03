#if 0 	///Temporary disabled
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "TVector3.h"
#include "StvGoneRejector.h"
#include "StvSeedConst.h"
#include "StvUtil/StvDebug.h"

enum {kFstAng=88,kMinLen=20,kMaxLen=50,kDivLen=3};
//enum {kFstAng=88,kMinLen=9,kMaxLen=30,kDivLen=3};
//enum {kFstAng=88,kMinLen=9,kMaxLen=90,kDivLen=3};
//enum {kFstAng=88,kMinLen=3,kMaxLen=50,kDivLen=3};
//enum {kFstAng=88,kMinLen=3,kMaxLen=50,kDivLen=5};
static const double kFstCos = cos(kFstAng*M_PI/180);
static const double kFstSin = cos(kFstAng*M_PI/180);

//_____________________________________________________________________________
//_____________________________________________________________________________
inline static float Dot(const float A[3],const float B[3])
{
   return A[0]*B[0]+A[1]*B[1]+A[2]*B[2];
}
//_____________________________________________________________________________
inline static float Dis2(const float A[3],const float B[3])
{
   float d = (A[0]-B[0])*(A[0]-B[0])
           + (A[1]-B[1])*(A[1]-B[1])
           + (A[2]-B[2])*(A[2]-B[2]);
   return d;
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
  mRxy2 = mPos[0]*mPos[0]+mPos[1]*mPos[1];
  mRxy  = sqrt(mRxy2);
  mErr = SEED_ERR(mRxy);

  double nor=0;
  if (dir) { 	//Direction defined
    Cop(mDir,dir);
    nor = pos[2]/mDir[2];
    mThet = (theta) ? theta : kFstAng*M_PI/180;
    assert(nor>0);
  } else   {	//Estimate dir as direction to 0,0,0

    double norL = 1./sqrt(mRxy2+pow(pos[2]-kZRange,2)); 
    double norR = 1./sqrt(mRxy2+pow(pos[2]+kZRange,2)); 
    double norQ = (norL+norR);
    mDir[0]= pos[0]*norQ;
    mDir[1]= pos[1]*norQ;
    mDir[2]= pos[2]*norQ +kZRange*(norR-norL);
    nor = sqrt(mDir[0]*mDir[0]+mDir[1]*mDir[1]+mDir[2]*mDir[2]);
    for (int i=0;i<3;i++) {mDir[i]/=(-nor);}
    mThet = acos(-(mDir[0]*pos[0]+mDir[0]*pos[1]+mDir[2]*(pos[2]+kZRange))*norR);

  }
  if (rad ) { mOutRad = rad;}	//rad is defined, use it
  else      { 			//rad is no defined, estimate it
    mOutRad = mRxy/kDivLen;
    if (mOutRad<kMinLen) mOutRad=kMinLen;
    if (mOutRad>kMaxLen) mOutRad=kMaxLen;
    mOutRad *= nor/mRxy;
  }
  mOutRad2 = (mOutRad+mErr)*(mOutRad+mErr);
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


  mPoint[2][0] =  dR;
  mPoint[2][2] =  dZ;

  mPoint[3][2] =  mOutRad;
  float tau = (mPoint[3][2]-mPoint[2][2])/mPoint[2][0];
  mPoint[3][0] =  mPoint[2][0]-tau*mPoint[2][2];
  for (int k=2;k<kNPonts-2;k+=2) {  
    for (int kk=k;kk<k+2;kk++) {
     mPoint[kk+2][0] =-mPoint[kk][1];
     mPoint[kk+2][1] = mPoint[kk][0];
     mPoint[kk+2][2] = mPoint[kk][2];
  } }

  memset(mPlane[0],0,sizeof(mPlane));
  mPlane[0][2]=-1; // mPlane[0][3]=0;
  mPlane[1][2]= 1;    mPlane[1][3]=mOutRad;

  TVector3 Vi(0,0,mOutRad/2);
  int ipl = 1;
 
//		Planes for 1st group of points
  for (int i1pnt=2;i1pnt<kNPonts;i1pnt+=2) {
    int i2pnt = (i1pnt+2); if (i2pnt>=kNPonts) i2pnt = 2;
    TVector3 V1(mPoint[i1pnt]);
    TVector3 V2(mPoint[i2pnt]);
    TVector3 Vn = V1.Cross(V2);
    Vn = Vn.Unit();
assert(Vn.Mag2()>=0.999);
    float *pl = mPlane[++ipl];
    pl[3] = 0;
    if (Vn.Dot(Vi)>0) 	{ Vn*=-1.;}
    for (int j=0;j<3;j++)	{ pl[j] = Vn[j];}
    assert(Vn.Dot(Vi)-pl[3]<=0);
  }
//		Planes for 2nd group of points
  for (int i0pnt=2;i0pnt<kNPonts;i0pnt+=2) {
    int i1pnt = (i0pnt+1); 
    int i2pnt = (i1pnt+2); if (i2pnt>=kNPonts) i2pnt = 2+1;
    TVector3 V0(mPoint[i0pnt]);
    TVector3 V1(mPoint[i1pnt]); V1-=V0;
    TVector3 V2(mPoint[i2pnt]); V2-=V0;
    TVector3 Vn = V1.Cross(V2);
    Vn = Vn.Unit();
    float *pl = mPlane[++ipl];
    pl[3] = Vn.Dot(V0);
    if (Vn.Dot(Vi)-pl[3]>0) 	{ Vn*=-1.; pl[3]*=-1.;}
    for (int j=0;j<3;j++)	{ pl[j] = Vn[j];}
    assert(Vn.Dot(Vi)-pl[3]<=0);
   }
 
 
 
{
   for (uint ip=0;ip<kNPonts;ip++) {
     assert(!Reject(mPoint[ip]));
   }  
}
  double sinL = mDir[2];
  assert(fabs(sinL)<1.001);
  if (sinL<-1) sinL=-1;
  if (sinL> 1) sinL= 1;
  double cosL = (1-sinL)*(1+sinL);
  cosL = sqrt(cosL);

  double cosP = 1,sinP=0;;
  if (cosL>1e-5) { cosP=mDir[0]/cosL; sinP=mDir[1]/cosL;}
  assert(fabs(cosP*cosP+sinP*sinP-1)<1e-5);

// Convert points into global sys
  for (int ip=0;ip<kNPonts;ip++) {
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
  for (int ip=0;ip<kNPlans;ip++) {
    float *p = mPlane[ip];

    float zz = p[2];
    p[2] = sinL*zz - cosL*p[0];
    p[0] = cosL*zz + sinL*p[0];

    float xx = p[0];  
    p[0] = cosP*xx - sinP*p[1];
    p[1] = sinP*xx + cosP*p[1];

    p[3] +=Dot(p,mPos);
  }
//		Test first 2 planes N == +-mDir
  assert(fabs(Dot(mPlane[0],mDir)+1)<1e-5);
  assert(fabs(Dot(mPlane[1],mDir)-1)<1e-5);


//	Create limits
// Convert points into global sys
  Set(mLim[0], 1e11,6);
  Set(mLim[1],-1e11,6);
  
  for (int ip=0;ip<kNPonts;ip++) {
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
   for (uint ip=0;ip<kNPonts;ip++) {
     assert(!Reject(mPoint[ip]));
   }  
}


}

//_____________________________________________________________________________
int StvGoneRejector::Reject(const float x[3]) const
{
//  if (Dis2(x,mPos)>mOutRad2) return 66;
  for (int ipl=0;ipl<kNPonts;ipl++) 
  {
    const float *p = mPlane[ipl];
    if (Dot(p,x)-p[3]>mErr) StvDebug::Count("KNNRej",10+ipl);
    if (Dot(p,x)-p[3]>mErr) return 77;
  }
  return 0;
}
//_____________________________________________________________________________
int StvGoneRejector::Test()
{
  StvGoneRejector R[2];
//  float pos[3] = { 50,100,150 };
  float pos[3] = { 50,0,150 };

  for (int jk=0;jk<2;jk++) {
    R[jk].Reset(pos);
    R[jk].Prepare();
    pos[2]*=-1;
  }   
//		Check points

  for (int ip1=0;ip1<kNPonts;ip1++) {
    int found = 0;
    for (int ip2=0;ip2<kNPonts;ip2++) {
      if (fabs(R[0].mPoint[ip1][0]-R[1].mPoint[ip2][0])>1e-5) continue;
      if (fabs(R[0].mPoint[ip1][1]-R[1].mPoint[ip2][1])>1e-5) continue;
      if (fabs(R[0].mPoint[ip1][2]+R[1].mPoint[ip2][2])>1e-5) continue;
      found = 1; break;
    }
    assert(found);
  }
  return 0;
}
#endif //0    
