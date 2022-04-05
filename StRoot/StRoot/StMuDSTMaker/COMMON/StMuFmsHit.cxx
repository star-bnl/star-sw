/***************************************************************************
 *
 * $Id: StMuFmsHit.cxx,v 1.1 2010/01/25 03:57:39 tone421 Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: StMuFmsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StMuFmsHit.cxx,v $
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#include "StMuFmsHit.h"

ClassImp(StMuFmsHit)

StMuFmsHit::StMuFmsHit() {
    mDetectorId  = 0;
    mChannel     = 0;
    mQTCrtSlotCh = 0;
    mAdc         = 0;
    mTdc         = 0;
    mEnergy      = 0;
}

StMuFmsHit::StMuFmsHit(unsigned short det, unsigned short ch,
                   unsigned short qtcrate, unsigned short qtslot,
                   unsigned short qtch, unsigned short adc,
                   unsigned short tdc, float e)
{
    setMuFmsHit(det, ch, qtcrate, qtslot, qtch, adc, tdc, e);
}

StMuFmsHit::~StMuFmsHit() {/* no op */}

unsigned short
StMuFmsHit::detectorId() const {return mDetectorId;}

unsigned short
StMuFmsHit::channel() const {return mChannel;}

unsigned short
StMuFmsHit::adc() const {return mAdc;}

unsigned short
StMuFmsHit::tdc() const {return mTdc;}

float
StMuFmsHit::energy() const {return mEnergy;}

unsigned short
StMuFmsHit::qtCrate()    const {return (mQTCrtSlotCh>>12) & 0x0f;}

unsigned short
StMuFmsHit::qtSlot()     const {return (mQTCrtSlotCh>>8 ) & 0x0f;}

unsigned short
StMuFmsHit::qtChannel()  const {return (mQTCrtSlotCh    ) & 0xff;}

void
StMuFmsHit::encodeQTCrtSlotCh(unsigned short qtcrate, unsigned short qtslot,
			    unsigned short qtch)
{
    mQTCrtSlotCh = (qtcrate<<12) + (qtslot<<8) + qtch;
}

void StMuFmsHit::setQtCrate(unsigned short val)    { mQTCrtSlotCh |= (val<<12); }
void StMuFmsHit::setQtSlot(unsigned short val)     { mQTCrtSlotCh |= (val<<8); }
void StMuFmsHit::setQtChannel(unsigned short val)  { mQTCrtSlotCh |= val; }
void StMuFmsHit::setDetectorId(unsigned short val) { mDetectorId   = val; }
void StMuFmsHit::setChannel(unsigned short val)    { mChannel      = val; }
void StMuFmsHit::setAdc(unsigned short val)        { mAdc          = val; }
void StMuFmsHit::setTdc(unsigned short val)        { mTdc          = val; }   
void StMuFmsHit::setEnergy(float val)              { mEnergy       = val; }
void StMuFmsHit::setMuFmsHit(unsigned short det, unsigned short ch,
                    unsigned short qtcrate, unsigned short qtslot,
                    unsigned short qtch, unsigned short adc,
                    unsigned short tdc, Float_t e){
    mDetectorId = det;
    mChannel    = ch;
    mAdc        = adc;
    mTdc        = tdc;
    mEnergy     = e;
    encodeQTCrtSlotCh(qtcrate, qtslot, qtch);
}

void StMuFmsHit::print(Option_t *option) const {cout<< *this <<endl;}

ostream& operator<<(ostream& os, const StMuFmsHit& v)
{
    return os <<"StMuFmsHit: mDetectorId\t"<<v.detectorId()
	    <<"\tmChannel\t"             <<v.channel()
	    <<"\tQTCrate\t"              <<v.qtCrate()
	    <<"\tQTSlot\t"               <<v.qtSlot()
	    <<"\tQTCh\t"                 <<v.qtChannel()
	    <<"\tmAdc\t"                 <<v.adc()
	    <<"\tmTdc\t"                 <<v.tdc()
	    <<"\tmEnergy\t"              <<v.energy();
}
