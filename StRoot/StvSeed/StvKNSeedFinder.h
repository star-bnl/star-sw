/// \File StvKNSeedFinder.h
/// \author Victorb Perev 01/2010
#ifndef StvKNSeedFinder_HH
#define StvKNSeedFinder_HH
#ifndef __NOSTV__
#include "Stv/StvSeedFinder.h"
#include "StvConeRejector.h"
#include "StvKNSeedSelector.h"
#endif
#include <map>
#include <vector>
/// \class StvKNSeedFinder

class StvHit;
class StvHits;
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
  int       Again();
  void      Reset();
  void      FeedBack(int success);
  void      Print(const char *opt="") const {;}
  const StvHits *GetHits() const; 	
  virtual void Show();
protected:

protected:
char mBeg[1];
int fIPass,fNSeeds[2],fNUsed[2];
char mMed[1];
StMultiKeyMap 		*fMultiHits;
StMultiKeyMapIter 	*fMultiIter;
Stv1stHitMap  		*f1stHitMap;
Stv1stHitMapIter  	*f1stHitMapIter;
char mEnd[1];
StvConeRejector       mRej;
StvKNSeedSelector     mSel;
#ifndef __NOSTV__
ClassDef(StvKNSeedFinder,0);
#endif
};


#endif
