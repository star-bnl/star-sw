//StvHitLoader.h

#ifndef StvHitLoader_HH
#define StvHitLoader_HH

#include <string>

#include "TNamed.h"
#include "StEvent/StEnumerations.h"


class StEvent;
class StHit;
class StvHit;
class StEventHitIter;
class StActorFunctor;
class StvStEventHitSelector;
class StvHitLoader : public TNamed 
{
 public:
    
    StvHitLoader(const char* name = "StHitsLoader");
    virtual ~StvHitLoader();
    void  Clear(const char* opt="");
    int   Init();
    void  SetHitSelector();
    void  SetHitActor(StActorFunctor *act)	{mHitLoadActor=act;}
    int   LoadHits(const StEvent *stev); 
    int   Finish();
    int   AddDetector(StDetectorId did);
    int   NumDetectors() const {return mNDets;};
 protected:
    StvHit *MakeStvHit(const StHit *stHit,UInt_t upath,int &sure); 
    int     TpcHitTest(const StHit *stHit);    
 private:
 int             mNDets;
 StDetectorId    mDetId;
 StEventHitIter *mHitIter;
 StvStEventHitSelector *mHitSelector;
 StActorFunctor *mHitLoadActor;
 ClassDef(StvHitLoader,0)
};

#endif
