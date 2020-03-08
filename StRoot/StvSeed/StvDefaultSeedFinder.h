/// \File StvDefaultSeedFinder.h
/// \author Victor Perev 01/2010
#ifndef StvDefaultSeedFinder_HH
#define StvDefaultSeedFinder_HH
#include "TVector3.h"
#include "Stv/StvSeedFinder.h"
#include <map>
#include <vector>
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
void SetSgn(int dir=1) { mSgn = dir; }
void SetVtx(const float vtx[3]) { 
  mIsVtx=0; memset(mVtx,0,sizeof(mVtx));
  if (vtx) {memcpy(mVtx,vtx,sizeof(mVtx));mIsVtx=1;};}

int  Reject(const float x[3],const void* hp);	// 0  :x accepted
						// >0 :x rejected
						// -1 =x     accepted and lims updated
private:
void UpdateLims();		//Update  limits

public:
//		Input data
char  mBeg[1];
int   mJst;
const void* mHp;
int   mIsVtx;
float mVtx[3];
float mErr;
float mRxy2;		//Rxy**2 of previous hit
float mRxy;		//Rxy    of previous hit
float mDelta;
float mMaxStep;
float mDir[3]; 		// track direction 
float mDirErr; 		// track direction error(same for each x,y,z)
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

const float *mX[100];	//array of hit coordinates pointers
const float *mHit;	//current (last accepted) hit coordinates
float mS[100];
int   mNPnt;
char  mEnd[1];
int   mSgn;
TVector3 mPnt[100];
};

class StvDefaultSeedFinder : public StvSeedFinder
{
enum { kNDejavu=4 };
public:
  StvDefaultSeedFinder(const char *name="Default");
   ~StvDefaultSeedFinder(){;}
  const THelixTrack_* NextSeed();
  void      Clear(const char *opt="");
  int       Again(int repeat);
  void      Reset();
  void      Print(const char *opt="") const 	{;}
   int      Reject(const float x[3])		{return mSel.Reject(x,0)>0;}	
void SetSgn(int dir=1) { fSgn = dir; mSel.SetSgn(dir);}
void SetVtx(const float vtx[3]) { StvSeedFinder::SetVtx(vtx); mSel.SetVtx(vtx);}

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
