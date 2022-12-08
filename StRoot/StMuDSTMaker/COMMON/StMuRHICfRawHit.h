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

		void setRunNumber(unsigned int run);
		void setEventNumber(unsigned int event);
		void setBunchNumber(unsigned int bunch);
		void setRunType(unsigned int type);
		void setTriggerNumber(unsigned int trigger);
		void setRunTime(Int_t idx, unsigned int time);
		void setRunTRGM(unsigned int trgm);

        void setPlateADC(Int_t tower, Int_t plate, Int_t range, Int_t adc);
        void setPlateADCDelay(Int_t tower, Int_t plate, Int_t range, Int_t adc);
		void setGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Int_t adc);
		void setTDC(Int_t idx, unsigned int val);
		void setCAD0(Int_t idx, unsigned int val);
		void setGPI0(Int_t idx, unsigned int val);
		void setGPI1(Int_t idx, unsigned int val);

		unsigned int getRunNumber();
		unsigned int getEventNumber();
		unsigned int getBunchNumber();
		unsigned int getRunType();
		unsigned int getTriggerNumber();
		unsigned int getRunTime(Int_t idx);
		unsigned int getRunTRGM();

		Int_t getPlateADC(Int_t tower, Int_t plate, Int_t range);
		Int_t getPlateADCDelay(Int_t tower, Int_t plate, Int_t range);
        Int_t getGSOBarADC(Int_t tower, Int_t layer, Int_t xy, Int_t bar);
		unsigned int getTDC(Int_t idx);
		unsigned int getCAD0(Int_t idx);
		unsigned int getGPI0(Int_t idx);
		unsigned int getGPI1(Int_t idx);

	private:
		unsigned int mRunNumber;
		unsigned int mEventNumber;
		unsigned int mBunchNumber;
		unsigned int mRunType;
		unsigned int mRHICfTrigger;
		unsigned int mRunTime[kRHICfNorder];
		unsigned int mRunTRGM;

		Int_t mPlateADC[kRHICfNtower][kRHICfNplate][kRHICfNrange];      // ADC of GSO plate
		Int_t mPlateADCDelay[kRHICfNtower][kRHICfNplate][kRHICfNrange]; // Delayed ADC of GSO plate (for pedestal)
		Int_t mGSOSmallADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];   // ADC of GSO bar of small tower 
		Int_t mGSOLargeADC[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];   // ADC of GSO bar of large tower
		unsigned int mTDC[kRHICfNtdc];   // TDC 
		unsigned int mCAD0[kRHICfNcad0]; // CAD0
		unsigned int mGPI0[kRHICfNgpi0]; // GPI0
		unsigned int mGPI1[kRHICfNgpi1]; // GPI1

    ClassDef(StMuRHICfRawHit,3)
};

#endif
