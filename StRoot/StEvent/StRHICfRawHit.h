#ifndef StRHICfRawHit_hh
#define StRHICfRawHit_hh

#include <TObject.h>
#include "StEnumerations.h"

class StRHICfRawHit : public TObject 
{
  public:
    StRHICfRawHit();
    ~StRHICfRawHit();

    void clear();

    void setPlateADC(Int_t tower, Int_t plate, Int_t range, Int_t adc);
    void setPlateADCDelay(Int_t tower, Int_t plate, Int_t range, Int_t adc);
    void setGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Int_t adc);
    void setTDC(Int_t idx, UInt_t val);
    void setCAD0(Int_t idx, UInt_t val);
    void setGPI0(Int_t idx, UInt_t val);
    void setGPI1(Int_t idx, UInt_t val);

    UShort_t getPlateADC(Int_t tower, Int_t plate, Int_t range);
    UShort_t getPlateADCDelay(Int_t tower, Int_t plate, Int_t range);
    UShort_t getGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar);
    UInt_t getTDC(Int_t idx);
    UInt_t getCAD0(Int_t idx);
    UInt_t getGPI0(Int_t idx);
    UInt_t getGPI1(Int_t idx);

  private:
    UShort_t mPlateADC[kRHICfNtower][kRHICfNplate][kRHICfNrange];      // ADC of GSO plate
    UShort_t mPlateADCDelay[kRHICfNtower][kRHICfNplate][kRHICfNrange]; // Delayed ADC of GSO plate (for pedestal)
    UShort_t mGSOSmallADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];   // ADC of GSO bar of small tower 
    UShort_t mGSOLargeADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];   // ADC of GSO bar of large tower
    UInt_t mTDC[kRHICfNtdc];   // TDC 
    UInt_t mCAD0[kRHICfNcad0]; // CAD0
    UInt_t mGPI0[kRHICfNgpi0]; // GPI0
    UInt_t mGPI1[kRHICfNgpi1]; // GPI1

  ClassDef(StRHICfRawHit,1)
};

#endif
