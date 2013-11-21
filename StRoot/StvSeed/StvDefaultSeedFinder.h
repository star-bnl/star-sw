/// \File StvDefaultSeedFinder.h
/// \author Victor Perev 01/2010
#ifndef StvDefaultSeedFinder_HH
#define StvDefaultSeedFinder_HH
#include "TVector3.h"
#include "Stv/StvSeedFinder.h"
#include <map>
#include <vector>
/// \class StvDefaultSeedFinder
//#define MultiPhiZMap 1
enum {kNKeys = 3};


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

void Reset() 				{mJst=-1;}
void Prepare();
void Update();
void AddHit(const float *x,const float *dir,float layer);
void SetErr(float err) 			{mErr=err;}


int  Reject(const float x[3],const void* hp);	// 0  :x accepted
					// >0 :x rejected
					//-1 =x     accepted and lims updated
private:
void UpdateLims();		//Update  limits

public:
//		Input data
char  mBeg[1];
int   mJst;
const void* mHp;
float mErr;
float mRxy2;
float mRxy;
float mDelta;
float mZ2;
float mDir[3]; 		// track direction
float mLayer;
const float *mHitDir; 	// hit plane direction
float mLen;		// cone length (height)
//		Calculated data

float mTan; 		// (tan(cone angle/2)
float mLim[4][kNKeys];
//		Output data
float mHitLen;
float mHitPrj;
float mMinPrj;
float mMinImp;

const float *mX[100];
const float *mHit;
float mS[100];
int   mNPnt;
char  mEnd[1];
TVector3 mPnt[100];
};

class StvDefaultSeedFinder : public StvSeedFinder
{
enum { kNDejavu=4 };
public:
  StvDefaultSeedFinder(const char *name="Default");
   ~StvDefaultSeedFinder(){;}
  const THelixTrack* NextSeed();
  void      Clear(const char *opt="");
  int       Again(int repeat);
  void      Reset();
  void      Print(const char *opt="") const 	{;}
  void      ShowIn();
   int      Reject(const float x[3])		{return mSel.Reject(x,0)>0;}	

protected:

private:
char mBeg[1];
int fIPass,fNSeeds[2],fNUsed[2];
char mMed[1];
int mNDejavu;
const StvHit *mDejavu[kNDejavu];

StMultiKeyMap 		*fMultiHits;
StMultiKeyMapIter 	*fMultiIter;
Stv1stHitMap  		*f1stHitMap;
Stv1stHitMapIter  	*f1stHitMapIter;
float const          	*m1stHit;
float  			 m1stDir[3]; 	// first track direction
char mEnd[1];
StvConeSelector       mSel;
ClassDef(StvDefaultSeedFinder,0);
};


#endif
