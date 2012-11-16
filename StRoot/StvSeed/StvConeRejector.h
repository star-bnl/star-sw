/// \File StvConeRejector.h
/// \author Victorb Perev 01/2010
/// \class StvConeRejector

#ifndef StvConeRejector_HH
#define StvConeRejector_HH


class StvConeRejector 
{
public:
  StvConeRejector ();
 ~StvConeRejector (){;}
void Reset(const float pos[3],const float dir[3]=0
          ,float len=0,float theta=0,float err=0);
void Prepare();

// 0=accepted
int  Reject(const float x[3]) const;	
				
private:

public:
//		Input data
char  mBeg[1];
float mPos[3]; 		// start position 
float mDir[3]; 		// track direction
float mLen;		// cone length (height)
float mLen2;		// mLen**2
float mThet;
float mCosThet;
float mSinThet;
float mErr;
float mLim[2][3];
char  mEnd[1];
};

#endif
