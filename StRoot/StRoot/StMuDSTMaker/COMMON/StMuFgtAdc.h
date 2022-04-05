/***************************************************************************
 *
 * $Id: StMuFgtAdc.h,v 1.1 2013/01/08 22:57:33 sangalin Exp $
 * Author: S. Gliske, Dec 2012
 *
 ***************************************************************************
 *
 * Description: the ADC value for a particular strip and a particular
 * time bin.  Setting these values in a seperate class (and seperate
 * TClonesArray) allows for a variable number of time bins.  Since the
 * ADC value is a short (2 bits) and packing usually is optimized for
 * 4 bits, we can use 2 more bits without effecting the storage space.
 * We choose to save the time bin number as a signed short.  For both
 * the ADC and time bin fields, negative values indicate errors or
 * uninitialized values.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtAdc.h,v $
 * Revision 1.1  2013/01/08 22:57:33  sangalin
 * Merged in FGT changes allowing for a variable number of timebins to be read out for each strip.
 *
 *
 **************************************************************************/

#ifndef _ST_MU_FGT_ADC_H_
#define _ST_MU_FGT_ADC_H_

#include <TObject.h>

class StMuFgtAdc : public TObject {
 public:
   // constructor
   StMuFgtAdc( Short_t adc = -1, Short_t tb = -1 );

   // defaults
   // StMuFgtAdc(const StMuFgtAdc&);  use default
   // StMuFgtAdc& operator=(const StMuFgtAdc&); use default
   // ~StMuFgtAdc(); use default

   // accessors
   Short_t getAdc() const;
   Short_t getTimeBin() const;

   // modifiers
   void setAdcAndTimeBin( Short_t adc, Short_t tb );
    
 protected:
   // data members
   Short_t mAdc;      // an ADC value
   Short_t mTimeBin;  // the time bin number

 private:   
   ClassDef(StMuFgtAdc,1);
}; 

// inline functions

inline StMuFgtAdc::StMuFgtAdc( Short_t adc, Short_t tb ) : mAdc(adc), mTimeBin( tb ) { /* */ };

inline Short_t StMuFgtAdc::getAdc()     const { return mAdc; };
inline Short_t StMuFgtAdc::getTimeBin() const { return mTimeBin; };

inline void StMuFgtAdc::setAdcAndTimeBin(  Short_t adc, Short_t tb ){ mAdc = adc; mTimeBin = tb; };

#endif
