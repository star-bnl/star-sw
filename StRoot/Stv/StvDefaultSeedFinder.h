/// \File StvDefaultSeedFinder.h
/// \author Victorb Perev 01/2010
#ifndef StvDefaultSeedFinder_HH
#define StvDefaultSeedFinder_HH
#include "StvSeedFinder.h"
#include <map>
#include <vector>
/// \class StvDefaultSeedFinder

class StvHit;
class StMultiKeyMap;
typedef std::multimap<float,StvHit*> Stv1stHitMap;
typedef Stv1stHitMap::iterator Stv1stHitMapIter;
class StMultiKeyMapIter;

class StvDefaultSelector 
{
public:
  StvDefaultSelector();
 ~StvDefaultSelector(){;}

void Prepare();
int  Reject(const float x[3]);	// 0  :x accepted
				// >0 :x rejected
				//-1 =x     accepted and lims updated
private:
void UpdateLims();		//Update XYZ limits

public:
//		Input data
char  mBeg[1];
const float *mX;
float mRxy2;
float mDir[3]; 		// track direction
float mAng; 		// (cone angle)/2
float mLen[2];		// l<mLen[0]: ACCEPTED and mLen[1]=mLen[0]
//		Calculated data

float mSin; 		// (sin(angle/2)
float mCos; 		// (cos(angle/2)
float mDelta[3]; 	// 
			// l<mLen[1]: ACCEPTED and mLen[1]=l
float mLim[2][3];
//		Output data
float mHitLen;
float mHitCos;
char  mEnd[1];


};

class StvDefaultSeedFinder : public StvSeedFinder
{
public:
  StvDefaultSeedFinder(const char *name="Default");
   ~StvDefaultSeedFinder(){;}
  const THelixTrack* NextSeed();
  void      Clear(const char *opt="");
  void      Reset();
  void      Print(const char *opt="") const {;}

protected:
  const THelixTrack* Approx();

private:
char mBeg[1];
int fIPass,fNSeeds[2],fNUsed[2];
char mMed[1];
StMultiKeyMap 		*fMultiHits;
StMultiKeyMapIter 	*fMultiIter;
Stv1stHitMap  		*f1stHitMap;
Stv1stHitMapIter  	*f1stHitMapIter;
char mEnd[1];
StvDefaultSelector       mSel;
ClassDef(StvDefaultSeedFinder,0);
};


#endif
