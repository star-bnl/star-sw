/***************************************************************************
 *
 * $Id: StMuFmsHit.h,v 1.3 2016/07/08 16:07:31 jdb Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: StMuFmsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StMuFmsHit.h,v $
 * Revision 1.3  2016/07/08 16:07:31  jdb
 * Did not increment the version number in last commit, doing so now. StMuFmsHit v1->v2 due to change from StObject->TObject
 *
 * Revision 1.2  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#ifndef StMuFmsHit_hh
#define StMuFmsHit_hh

#include <TObject.h>

#include "Stiostream.h"

class StMuFmsHit : public TObject {
public:
    StMuFmsHit();
    StMuFmsHit(unsigned short det, unsigned short ch,
             unsigned short qtcrate, unsigned short qtslot,
             unsigned short qtch, unsigned short adc,
             unsigned short tdc, float e);
    ~StMuFmsHit();
    
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
    void setMuFmsHit(unsigned short det, unsigned short ch,
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
    
    ClassDef(StMuFmsHit,2)
};

ostream& operator<<(ostream&, const StMuFmsHit&);
#endif
