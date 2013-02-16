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
  mCosThet = cos(mThet);
  mSinThet = sin(mThet);
  mErr = (err)? err : SEED_ERR(nor);
}

//_____________________________________________________________________________
void StvConeRejector::Prepare()
{
/// 	Calculation of brik sarrounding our cone sector
///	Parameters of brik are used for selection in multy key iterator
///	of hits

  for (int ix=0;ix<3;ix++) { mLim[0][ix]=999; mLim[1][ix]= -999;} 

  TVector3 myZ(mDir),myX(myZ.Orthogonal()),myY(myZ.Cross(myX));		
  TVector3 pos[2];		
  pos[0]= TVector3(mPos); 		
  pos[1]= pos[0]+myZ*(mCosThet*mLen);		
  double fak[2]={mErr,mLen*mSinThet};
  for (int ix=0;ix<3;ix++) {			//loop over x,y,z
    double delta = sqrt(myX[ix]*myX[ix]+myY[ix]*myY[ix]);
    for (int iBegEnd=0;iBegEnd<2;iBegEnd++){	// loop over begin&end of cone
      for (int jk=-1;jk<=1;jk+=2) {		// loop over -1,+1 sign
        double dlt = delta*fak[iBegEnd]*jk;
        double qwe = pos[iBegEnd][ix]+dlt;
        if (mLim[0][ix]>qwe)  mLim[0][ix]=qwe;        
        if (mLim[1][ix]<qwe)  mLim[1][ix]=qwe;        
      }// end sign loop
    }// end of begEnd
  }// end of x,y,z

  for (int ix=0;ix<3;ix++) {			//loop over x,y,z
    for (int jk=-1;jk<=1;jk+=2) {		// loop over -1,+1 sign
      if (myZ[ix]*jk<mCosThet) continue;
      double qwe = mPos[ix]+mLen*jk;
      if (mLim[0][ix]>qwe)  mLim[0][ix]=qwe;        
      if (mLim[1][ix]<qwe)  mLim[1][ix]=qwe;        
    }//end of sign loop
  }//end of x,y,z loop

}   
//_____________________________________________________________________________
int StvConeRejector::Reject(const float x[3]) const
{
  float xx[3];
  if (x[0]*x[0]+x[1]*x[1]>mRad2 - 2*mRad*mErr) return 4;
  Sub(xx,x,mPos);
  float r2 = xx[0]*xx[0]+xx[1]*xx[1]+xx[2]*xx[2];
  if (r2>mLen2)	return 1;
  float myX = Dot(xx,mDir);
  if (myX <0) 	return 2;
  float myY = (r2-myX*myX);
  myY = (myY>0)? sqrt(myY):0;
  if (myY*mCosThet-myX*mSinThet>mErr) return 3;
  return 0;
}
