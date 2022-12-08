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
		void setTDC(Int_t idx, unsigned int val);
		void setCAD0(Int_t idx, unsigned int val);
		void setGPI0(Int_t idx, unsigned int val);
		void setGPI1(Int_t idx, unsigned int val);

		Int_t getPlateADC(Int_t tower, Int_t plate, Int_t range);
		Int_t getPlateADCDelay(Int_t tower, Int_t plate, Int_t range);
        Int_t getGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar);
		unsigned int getTDC(Int_t idx);
		unsigned int getCAD0(Int_t idx);
		unsigned int getGPI0(Int_t idx);
		unsigned int getGPI1(Int_t idx);

	private:
		Int_t mPlateADC[kRHICfNtower][kRHICfNplate][kRHICfNrange];      // ADC of GSO plate
		Int_t mPlateADCDelay[kRHICfNtower][kRHICfNplate][kRHICfNrange]; // Delayed ADC of GSO plate (for pedestal)
		Int_t mGSOSmallADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];   // ADC of GSO bar of small tower 
		Int_t mGSOLargeADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];   // ADC of GSO bar of large tower
		unsigned int mTDC[kRHICfNtdc];   // TDC 
		unsigned int mCAD0[kRHICfNcad0]; // CAD0
		unsigned int mGPI0[kRHICfNgpi0]; // GPI0
		unsigned int mGPI1[kRHICfNgpi1]; // GPI1

    ClassDef(StRHICfRawHit,1)
};

#endif
