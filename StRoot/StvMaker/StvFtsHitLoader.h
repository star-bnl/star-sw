//StvFtsHitLoader.h

#ifndef StvFtsHitLoader_HH
#define StvFtsHitLoader_HH
#include "StvHitLoader.h"

class StvFtsHitLoader : public StvHitLoader
 
{
 public:
    
 StvFtsHitLoader(const char* name = "StFtsHitsLoader"):StvHitLoader(name){}
    virtual ~StvFtsHitLoader(){;}
 protected:
 int MakeStvHit(const StHit *stHit,UInt_t upath,int &sure,StvHit *stvHit=0); 
 private:
 ClassDef(StvFtsHitLoader,0)
};

#endif
