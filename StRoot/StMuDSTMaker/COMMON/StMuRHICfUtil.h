#ifndef StMuRHICfUtil_h
#define StMuRHICfUtil_h

#include "StObject.h"
#include "StRoot/StEvent/StEnumerations.h"

class StRHICfCollection;
class StRHICfRawHit;
class StRHICfHit;
class StRHICfPoint;
class StMuRHICfCollection;
class StMuRHICfRawHit;
class StMuRHICfHit;
class StMuRHICfPoint;
class StMuDst;

class StMuRHICfUtil : public StObject
{
  public:
    StMuRHICfUtil();
    ~StMuRHICfUtil();

    StMuRHICfCollection* getMuRHICf(StRHICfCollection*);
    StRHICfCollection* getRHICf(StMuRHICfCollection*);

    void fillMuRHICf(StMuRHICfCollection*,StRHICfCollection*);
    void fillRHICf(StRHICfCollection*,StMuRHICfCollection*);

    void fillMuRHICfRawHit(StMuRHICfRawHit*,StRHICfRawHit*);
    void fillMuRHICfHit(StMuRHICfHit*,StRHICfHit*);
    void fillMuRHICfPoint(StMuRHICfPoint*,StRHICfPoint*);

    void fillRHICfRawHit(StRHICfRawHit*,StMuRHICfRawHit*);
    void fillRHICfHit(StRHICfHit*,StMuRHICfHit*);
    void fillRHICfPoint(StRHICfPoint*,StMuRHICfPoint*);

  private:
    Int_t checkGSOBarSize(Int_t tower);

  ClassDef(StMuRHICfUtil,0)
};

#endif
