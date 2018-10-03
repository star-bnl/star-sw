/***************************************************************************
 *
 * $Id: StFcsHit.h,v 1.1.2.3 2018/10/03 18:45:15 jwebb Exp $
 *
 * Author: Akio Ogawaa, Aug 2018
 ***************************************************************************
 *
 * Description: StFcsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StFcsHit.h,v $
 * Revision 1.1.2.3  2018/10/03 18:45:15  jwebb
 * Unused code, but checkin latest version for future
 *
 *
 **************************************************************************/
#ifndef StFcsHit_hh
#define StFcsHit_hh

#include "Stiostream.h"
#include "StObject.h"

class StFcsHit : public StObject {
public:
    StFcsHit();
    StFcsHit(unsigned short det, unsigned short id,
             unsigned short crate, unsigned short slot, unsigned short ch, 
             unsigned short timebin, 
	     unsigned short adc, float e);
    ~StFcsHit();
    
    unsigned short detectorId() const;
    unsigned short id() const;
    unsigned short crate() const;
    unsigned short slot() const;
    unsigned short channel() const;
    unsigned short timebin() const;
    unsigned short adc() const;
    float          energy() const;
    
    void setDetectorId(unsigned short);
    void setId(unsigned short);
    void setCrate(unsigned short);
    void setSlot(unsigned short);
    void setChannel(unsigned short);
    void setTimebin(unsigned short);
    void setAdc(unsigned short);
    void setEnergy(float);
    void setFcsHit(unsigned short det, unsigned short id,
                   unsigned short crate, unsigned short slot, unsigned short ch, 
		   unsigned short timebin,
		   unsigned short adc, float e);
    
    void print(Option_t *option="") const;

protected:
    
protected:
    UShort_t mDetectorId=0;  // Detector Id
    UShort_t mId=0;          // Channel id in the detector
    UShort_t mCrtSlotCh=0;   // 4 bits for Crate and Slot, 8 bits for channal
    UShort_t mTimebin=0;     // Timebin (0 for integral for now)
    UShort_t mAdc=0;         // ADC values
    Float_t  mEnergy=0;      // corrected energy
    
    ClassDef(StFcsHit,1)
};

#endif
