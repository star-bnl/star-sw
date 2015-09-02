/***************************************************************************
 *
 * $Id: StFmsHit.cxx,v 2.2 2015/09/01 21:01:47 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description: StFmsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StFmsHit.cxx,v $
 * Revision 2.2  2015/09/01 21:01:47  ullrich
 * Minor changes to format of print statments and \nchange to naming of data member.
 *
 * Revision 2.1  2010/01/08 22:42:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFmsHit.h"

ClassImp(StFmsHit)

StFmsHit::StFmsHit() {
    mDetectorId  = 0;
    mChannel     = 0;
    mQTCrtSlotCh = 0;
    mAdc         = 0;
    mTdc         = 0;
    mEnergy      = 0;
}

StFmsHit::StFmsHit(unsigned short det, unsigned short ch,
                   unsigned short qtcrate, unsigned short qtslot,
                   unsigned short qtch, unsigned short adc,
                   unsigned short tdc, float e)
{
    setFmsHit(det, ch, qtcrate, qtslot, qtch, adc, tdc, e);
}

StFmsHit::~StFmsHit() {/* no op */}

unsigned short
StFmsHit::detectorId() const {return mDetectorId;}

unsigned short
StFmsHit::channel() const {return mChannel;}

unsigned short
StFmsHit::adc() const {return mAdc;}

unsigned short
StFmsHit::tdc() const {return mTdc;}

float
StFmsHit::energy() const {return mEnergy;}

unsigned short
StFmsHit::qtCrate()    const {return (mQTCrtSlotCh>>12) & 0x0f;}

unsigned short
StFmsHit::qtSlot()     const {return (mQTCrtSlotCh>>8 ) & 0x0f;}

unsigned short
StFmsHit::qtChannel()  const {return (mQTCrtSlotCh    ) & 0xff;}

void
StFmsHit::encodeQTCrtSlotCh(unsigned short qtcrate, unsigned short qtslot,
			    unsigned short qtch)
{
    mQTCrtSlotCh = (qtcrate<<12) + (qtslot<<8) + qtch;
}

void StFmsHit::setQtCrate(unsigned short val)    { mQTCrtSlotCh |= (val<<12); }
void StFmsHit::setQtSlot(unsigned short val)     { mQTCrtSlotCh |= (val<<8); }
void StFmsHit::setQtChannel(unsigned short val)  { mQTCrtSlotCh |= val; }
void StFmsHit::setDetectorId(unsigned short val) { mDetectorId   = val; }
void StFmsHit::setChannel(unsigned short val)    { mChannel      = val; }
void StFmsHit::setAdc(unsigned short val)        { mAdc          = val; }
void StFmsHit::setTdc(unsigned short val)        { mTdc          = val; }   
void StFmsHit::setEnergy(float val)              { mEnergy       = val; }
void StFmsHit::setFmsHit(unsigned short det, unsigned short ch,
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

void StFmsHit::print(Option_t *option) const {cout<< *this <<endl;}

ostream& operator<<(ostream& os, const StFmsHit& v)
{
  return os << Form("StFmsHit: detId=%2d ch=%3d QTCrt=%1d QTSlot=%2d QTCh=%2d adc=%4d tdc=%4d E=%6.2f",
		    v.detectorId(),v.channel(),v.qtCrate(),v.qtSlot(),v.qtChannel()
		    ,v.adc(),v.tdc(),v.energy());
}
