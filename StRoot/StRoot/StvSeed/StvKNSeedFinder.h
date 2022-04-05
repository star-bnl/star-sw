/// \File StvKNSeedFinder.h
/// \author Victorb Perev 01/2010
#ifndef StvKNSeedFinder_HH
#define StvKNSeedFinder_HH
#ifndef __NOSTV__
#include "Stv/StvSeedFinder.h"
#endif
#include "StvSeedConst.h"
#include "StvConeRejector.h"
#include "StvKNSeedSelector.h"
#include <map>
#include <vector>
/// \class StvKNSeedFinder

class StvHit;
class StvHits;
class StvTrack;
class StMultiKeyMap;
typedef std::multimap<float,StvHit*> Stv1stHitMap;
typedef Stv1stHitMap::iterator Stv1stHitMapIter;
class StMultiKeyMapIter;


class StvKNSeedFinder : public StvSeedFinder
{
public:
  StvKNSeedFinder(const char *name="KN");
   ~StvKNSeedFinder(){;}
  const THelixTrack* NextSeed();
  void      Clear(const char *opt="");
  int       Again(int repeat);
  void      Reset();
  void      FeedBack(const StvTrack *tk);
  void      Print(const char *opt="") const {;}
  const StvHits *GetHits() const; 	
  const float *Eigen(){return mSel.Eigen();}
  virtual void Show();
protected:

protected:
char mBeg[1];
int fNSeeds[2],fNUsed[2];
StvHit *fstHit;
char mMed[1];
StMultiKeyMap 		*fMultiHits;
StMultiKeyMapIter 	*fMultiIter;
Stv1stHitMap  		*f1stHitMap;
Stv1stHitMapIter  	*f1stHitMapIter;
char mEnd[1];
StvConeRejector       mRej;
StvKNSeedSelector     mSel;
ClassDef(StvKNSeedFinder,0);
};


#endif
