/// \File StvDefaultSeedFinder.h
/// \author Victorb Perev 01/2010
#ifndef StvDefaultSeedFinder_HH
#define StvDefaultSeedFinder_HH
#include "Stv/StvSeedFinder.h"
#include <map>
#include <vector>
/// \class StvDefaultSeedFinder

class StvHit;
class StMultiKeyMap;
typedef std::multimap<float,StvHit*> Stv1stHitMap;
typedef Stv1stHitMap::iterator Stv1stHitMapIter;
class StMultiKeyMapIter;

class StvConeSelector 
{
public:
  StvConeSelector();
 ~StvConeSelector(){;}

void Reset() 			{mJst=-1;}
void Prepare();
void Update();
void AddHit(const float *x,const float *dir);
void SetErr(float err) 		{mErr=err;}
void SetXYStep(float stp) 	{mZStep=0;   mXYStep=stp;}
void SetZStep (float stp) 	{mZStep=stp; mXYStep=0  ;}


int  Reject(const float x[3]);	// 0  :x accepted
				// >0 :x rejected
				//-1 =x     accepted and lims updated
private:
void UpdateLims();		//Update XYZ limits

public:
//		Input data
char  mBeg[1];
int   mJst;
float mErr;
float mXYStep;
float mZStep;
float mRxy2;
float mRxy;
float mDelta;
float mZ2;
float mDir[3]; 		// track direction
const float *mHitDir; 	// hit plane direction
float mLen;		// cone length (height)
//		Calculated data

float mTan; 		// (tan(cone angle/2)
float mLim[2][3];
//		Output data
float mHitLen;
float mHitPrj;
const float *mX[100];
const float *mHit;
float mS[100];
char  mEnd[1];
};

class StvDefaultSeedFinder : public StvSeedFinder
{
public:
  StvDefaultSeedFinder(const char *name="Default");
   ~StvDefaultSeedFinder(){;}
  const THelixTrack* NextSeed();
  void      Clear(const char *opt="");
  int       Again();
  void      Reset();
  void      Print(const char *opt="") const {;}

protected:

private:
char mBeg[1];
int fIPass,fNSeeds[2],fNUsed[2];
char mMed[1];
StMultiKeyMap 		*fMultiHits;
StMultiKeyMapIter 	*fMultiIter;
Stv1stHitMap  		*f1stHitMap;
Stv1stHitMapIter  	*f1stHitMapIter;
char mEnd[1];
StvConeSelector       mSel;
ClassDef(StvDefaultSeedFinder,0);
};


#endif
