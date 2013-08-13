/// \File StiDefaultSeedFinder.h
/// \author Victorb Perev 01/2010
#ifndef StiDefaultSeedFinder_HH
#define StiDefaultSeedFinder_HH
#include "StiSeedFinder.h"
#include <map>
#include <vector>
/// \class StiDefaultSeedFinder

class StiHit;
class StMultiKeyMap;
typedef std::multimap<float,StiHit*> Sti1stHitMap;
typedef Sti1stHitMap::iterator Sti1stHitMapIter;
class StMultiKeyMapIter;

class StiDefaultSelector 
{
public:
  StiDefaultSelector();
 ~StiDefaultSelector(){;}

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

class StiDefaultSeedFinder : public StiSeedFinder
{
public:
  StiDefaultSeedFinder(const char *name="Default");
   ~StiDefaultSeedFinder(){;}
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
Sti1stHitMap  		*f1stHitMap;
Sti1stHitMapIter  	*f1stHitMapIter;
char mEnd[1];
StiDefaultSelector       mSel;
ClassDef(StiDefaultSeedFinder,0);
};


#endif
