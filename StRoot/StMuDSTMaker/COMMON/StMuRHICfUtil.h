#ifndef StMuRHICfUtil_h
#define StMuRHICfUtil_h

#include "StObject.h"
#include "StRoot/StEvent/StEnumerations.h"

class StRHICfCollection;
class StMuRHICfCollection;
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

  private:
    Int_t checkGSOBarSize(Int_t tower);

    void fillMuRHICfRawHit(StMuRHICfCollection*, StRHICfCollection*);
    void fillMuRHICfHit(StMuRHICfCollection*, StRHICfCollection*);
    void fillMuRHICfPoint(StMuRHICfCollection*, StRHICfCollection*);

    void fillRHICfRawHit(StRHICfCollection*, StMuRHICfCollection*);
    void fillRHICfHit(StRHICfCollection*, StMuRHICfCollection*);
    void fillRHICfPoint(StRHICfCollection*, StMuRHICfCollection*);

  ClassDef(StMuRHICfUtil,0)
};

#endif
