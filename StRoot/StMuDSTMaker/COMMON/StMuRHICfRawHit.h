#ifndef StMuRHICfRawHit_hh
#define StMuRHICfRawHit_hh

#include <TObject.h>
#include "StRoot/StEvent/StEnumerations.h"

class StMuRHICfRawHit : public TObject 
{
  public:
    StMuRHICfRawHit();
    ~StMuRHICfRawHit();

    void clear();

    void setBunchNumber(UInt_t bunch);
    void setRunType(UInt_t type);
    void setTriggerNumber(UInt_t trigger);
    void setRunTime(Int_t idx, UInt_t time);
    void setRunTRGM(UInt_t trgm);

    void setPlateADC(Int_t tower, Int_t plate, Int_t range, Int_t adc);
    void setPlateADCDelay(Int_t tower, Int_t plate, Int_t range, Int_t adc);
    void setGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Int_t adc);
    void setTDC(Int_t idx, UInt_t val);
    void setCAD0(Int_t idx, UInt_t val);
    void setGPI0(Int_t idx, UInt_t val);
    void setGPI1(Int_t idx, UInt_t val);

    UInt_t getBunchNumber();
    UInt_t getRunType();
    UInt_t getTriggerNumber();
    UInt_t getRunTime(Int_t idx);
    UInt_t getRunTRGM();

    UShort_t getPlateADC(Int_t tower, Int_t plate, Int_t range);
    UShort_t getPlateADCDelay(Int_t tower, Int_t plate, Int_t range);
    UShort_t getGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar);
    UInt_t getTDC(Int_t idx);
    UInt_t getCAD0(Int_t idx);
    UInt_t getGPI0(Int_t idx);
    UInt_t getGPI1(Int_t idx);

  private:
    UInt_t mBunchNumber;
    UInt_t mRunType;
    UInt_t mRHICfTrigger;
    UInt_t mRunTime[kRHICfNorder];
    UInt_t mRunTRGM;

    UShort_t mPlateADC[kRHICfNtower][kRHICfNplate][kRHICfNrange];      // ADC of GSO plate
    UShort_t mPlateADCDelay[kRHICfNtower][kRHICfNplate][kRHICfNrange]; // Delayed ADC of GSO plate (for pedestal)
    UShort_t mGSOSmallADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];   // ADC of GSO bar of small tower 
    UShort_t mGSOLargeADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];   // ADC of GSO bar of large tower
    UInt_t mTDC[kRHICfNtdc];   // TDC 
    UInt_t mCAD0[kRHICfNcad0]; // CAD0
    UInt_t mGPI0[kRHICfNgpi0]; // GPI0
    UInt_t mGPI1[kRHICfNgpi1]; // GPI1

  ClassDef(StMuRHICfRawHit,1)
};

#endif
