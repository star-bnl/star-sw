/// \File StvGoneRejector.h
/// \author Victorb Perev 01/2010
/// \class StvGoneRejector

#ifndef StvGoneRejector_HH
#define StvGoneRejector_HH


class StvGoneRejector 
{
public:
  StvGoneRejector ();
 ~StvGoneRejector (){;}
void Reset(const float pos[3],const float dir[3]=0
          ,float len=0,float theta=0);
void Prepare();

// 0=accepted
int  Reject(const float x[3]) const;	
				
private:

public:
//		Input data
char  mBeg[1];
float mPos[3]; 		// start position 
float mDir[3]; 		// track direction
float mOutRad;		// cone radius of limitation (similar to height)
float mOutRad2;		// mOutRad**2
float mRxy;		// Rxy    ofstart position
float mRxy2;		// Rxy**2 
float mThet;		// 1/2 of cone angle
float mCos;		// cos(mThet)
float mSin;		// sin(mThet)
float mTan2;		// tan(mThet)**2
float mErr;		// 3d accuracy to be inside of cone
float mPlane[10][4];  	// Planes 	[0]=at start track & ort to it
 			// 		[1]=at end   track & ort to it
 			// 		[>1]=surroundings
			// Plane format:
			// mPlane[0]*x+mPlane[1]*y+mPlane[2]*z<mPlane[3]
			// point inside of gone
float mPoint[ 6][3];	// points surrounding Gone
float mLim[2][6];       // xyz min and xyz max of cube around the gone
char  mEnd[1];
};

#endif
