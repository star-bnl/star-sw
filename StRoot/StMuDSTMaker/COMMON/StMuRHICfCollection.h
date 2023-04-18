#ifndef StMuRHICfCollection_hh
#define StMuRHICfCollection_hh

#include "St_base/StObject.h"
#include "TClonesArray.h"

class StMuRHICfRawHit;
class StMuRHICfHit;
class StMuRHICfPoint;

class StMuRHICfCollection : public TObject 
{
  public:
    StMuRHICfCollection();
    ~StMuRHICfCollection();    

    void init();

    unsigned int  numberOfPoints() const;

    StMuRHICfRawHit* addRawHit();
    StMuRHICfHit* addHit();
    StMuRHICfPoint* addPoint();
    
    StMuRHICfRawHit* getRawHit();
    StMuRHICfHit* getHit();
    StMuRHICfPoint* getPoint(Int_t index);

    TClonesArray* getPointArray();

    void setRHICfRawHitArray(TClonesArray *array);
    void setRHICfHitArray(TClonesArray *array);
    void setRHICfPointArray(TClonesArray* array);

  private:
    TClonesArray* mRawHit;
    TClonesArray* mHit;
    TClonesArray* mPoint;

  ClassDef(StMuRHICfCollection,1)
};

#endif
