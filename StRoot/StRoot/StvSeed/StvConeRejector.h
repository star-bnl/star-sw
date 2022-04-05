/// \File StvConeRejector.h
/// \author Victorb Perev 01/2010
/// \class StvConeRejector

#ifndef StvConeRejector_HH
#define StvConeRejector_HH
#include "StvSeed/StvSeedConst.h"

class StvConeRejector 
{
public:
  StvConeRejector ();
 ~StvConeRejector (){;}
void Reset(const float pos[3],const float dir[3]=0
          ,float len=0,float theta=0,float err=0);
void Prepare();
const Mtx33F_t &GetDir() const { return mDir;}

// 0=accepted
int  Reject(const float x[3]) const;	
int  TestIt() const;				
private:

public:
//		Input data
char  mBeg[1];
float mPos[3]; 		// start position 
float mDir[3][3]; 	// track direction + orthogonal axises
float mOutRad;		// cone radius of limitation (similar to height)
float mOutRad2;		// mOutRad**2
float mRxy;		// Rxy    ofstart position
float mRxy2;		// Rxy**2 
float mR2;		// R**2 
float mThet;		// 1/2 of cone angle
float mCos;		// cos(mThet)
float mSin;		// sin(mThet)
float mTan2;		// tan(mThet)**2
float mErr;		// 3d accuracy to be inside of cone
float mLim[2][3];  	// xyz min and xyz max of cube around the cone
char  mEnd[1];
};

#endif
