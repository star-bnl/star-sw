#ifndef StRHICfCollection_hh
#define StRHICfCollection_hh

#include <vector>

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"

#include "StRHICfRawHit.h"
#include "StRHICfHit.h"
#include "StRHICfPoint.h"

class StRHICfCollection : public StObject 
{
  public:
    StRHICfCollection();
    ~StRHICfCollection();

    void clear();

    // main RHICf data structure
    StRHICfRawHit* rawHitCollection();
    StRHICfHit* hitCollection();

    void addPoint(StRHICfPoint* pointColl); 
    void addPointCollection(std::vector<StRHICfPoint*> coll);
    std::vector<StRHICfPoint*>& pointCollection();    // Return the point list
    const std::vector<StRHICfPoint*>& pointCollection() const;

    // run header 
    void isAllSave();
    void setRHICfRunNumber(UInt_t run);
    void setRHICfEventNumber(UInt_t event);
    void setBunchNumber(UInt_t bunch);
    void setRunType(UInt_t type);
    void setTriggerNumber(UInt_t trigger);
    void setRunTime(Int_t idx, UInt_t time);
    void setRunTRGM(UInt_t trgm);

    UInt_t numberOfPoints() const;
    UInt_t getRHICfRunNumber() const;
    UInt_t getRHICfEventNumber() const;
    UInt_t getBunchNumber();
    UInt_t getRunType();
    UInt_t getTriggerNumber();
    UInt_t getRunTime(Int_t idx);
    UInt_t getRunTRGM();

  private:
    StRHICfRawHit* mRHICfRawHitColl; 
    StRHICfHit* mRHICfHitColl; 
    std::vector<StRHICfPoint*> mRHICfPointColl; 

    // Important note: the _RHICf_ Run and Event numbers are distinct from the _STAR_ Run and Event numbers,
    // originating from RHICf's own raw data acquisition and studies outside of the STAR framework
    UInt_t mRHICfRunNumber;
    UInt_t mRHICfEventNumber;
    UInt_t mBunchNumber;
    UInt_t mRunType;
    UInt_t mRHICfTrigger;
    UInt_t mRunTime[kRHICfNorder];
    UInt_t mRunTRGM;

  ClassDef(StRHICfCollection,2)
};

#endif
