/***************************************************************************
 *
 * $Id: StFmsHit.h,v 2.1 2010/01/08 22:42:31 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description: StFmsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StFmsHit.h,v $
 * Revision 2.1  2010/01/08 22:42:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFmsHit_hh
#define StFmsHit_hh

#include "Stiostream.h"
#include "StObject.h"

class StFmsHit : public StObject {
public:
    StFmsHit();
    StFmsHit(unsigned short det, unsigned short ch,
             unsigned short qtcrate, unsigned short qtslot,
             unsigned short qtch, unsigned short adc,
             unsigned short tdc, float e);
    ~StFmsHit();
    
    unsigned short detectorId() const;
    unsigned short channel() const;
    unsigned short qtCrate() const;
    unsigned short qtSlot() const;
    unsigned short qtChannel() const;
    unsigned short adc() const;
    unsigned short tdc() const;
    float          energy() const;
    
    void setDetectorId(unsigned short);
    void setChannel(unsigned short);
    void setQtCrate(unsigned short);
    void setQtSlot(unsigned short);
    void setQtChannel(unsigned short);
    void setAdc(unsigned short);
    void setTdc(unsigned short);
    void setEnergy(float);
    void setFmsHit(unsigned short det, unsigned short ch,
                   unsigned short qtcrate, unsigned short qtslot,
                   unsigned short qtch, unsigned short adc,
                   unsigned short tdc, float e);
    
  void print(Option_t *option="") const;

protected:
    void encodeQTCrtSlotCh(unsigned short qtcrate, unsigned short qtslot, unsigned short qtch);
    
protected:
    UShort_t mDetectorId;  // Detector Id
    UShort_t mChannel;     // Channel in the detector
    UShort_t mQTCrtSlotCh; // QT Crate/Slot/Ch, 4 bits for Crate and Slot, 8 bits for channal
    UShort_t mAdc;         // ADC values
    UShort_t mTdc;         // TDC values
    Float_t  mEnergy;      // corrected energy
    
    ClassDef(StFmsHit,1)
};

ostream& operator<<(ostream&, const StFmsHit&);
#endif
