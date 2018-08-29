/***************************************************************************
 *
 * $Id: StFcsHit.cxx,v 1.1.2.2 2018/08/29 14:52:37 jwebb Exp $
 *
 * Author: Akio Ogawa, Aug 2018
 ***************************************************************************
 *
 * Description: StFcsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StFcsHit.cxx,v $
 * Revision 1.1.2.2  2018/08/29 14:52:37  jwebb
 * o Victor corrected issues with FCS hit dependency, enumerations, definitions...
 *
 * o Added missing implementations for fcsCollection() and setFcsCollection(...).
 *
 * Revision 1.1.2.1  2018/08/24 15:14:36  jwebb
 *
 * Forward Calorimeter System (HCAL and WCAL) added to event model.
 *
 *
 **************************************************************************/
#include "StFcsHit.h"

ClassImp(StFcsHit)

StFcsHit::StFcsHit() {}

StFcsHit::StFcsHit(unsigned short det, unsigned short id,
                   unsigned short crate, unsigned short slot,unsigned short ch, 
		   unsigned short timebin,
                   unsigned short adc, float e)
{
    setFcsHit(det, ch, crate, slot, ch, timebin, adc, e);
} 

StFcsHit::~StFcsHit() {/* no op */}

unsigned short StFcsHit::id() const {return mId;}
unsigned short StFcsHit::crate()    const {return (mCrtSlotCh>>12) & 0x0f;}
unsigned short StFcsHit::slot()     const {return (mCrtSlotCh>>8 ) & 0x0f;}
unsigned short StFcsHit::channel()  const {return (mCrtSlotCh    ) & 0xff;}
unsigned short StFcsHit::timebin() const {return mTimebin;}
unsigned short StFcsHit::adc() const {return mAdc;}
float StFcsHit::energy() const {return mEnergy;}

void StFcsHit::setCrate(unsigned short val)      { mCrtSlotCh |= (val<<12); }
void StFcsHit::setSlot(unsigned short val)       { mCrtSlotCh |= (val<<8); }
void StFcsHit::setChannel(unsigned short val)    { mCrtSlotCh |= val; }
void StFcsHit::setId(unsigned short val)         { mId         = val; }
void StFcsHit::setAdc(unsigned short val)        { mAdc        = val; }
void StFcsHit::setTimebin(unsigned short val)    { mTimebin    = val; }   
void StFcsHit::setEnergy(float val)              { mEnergy     = val; }
void StFcsHit::setFcsHit(unsigned short det, unsigned short id,
			 unsigned short crate, unsigned short slot, unsigned short ch, 
			 unsigned short timebin,
			 unsigned short adc, Float_t e){
    mId         = id;
    mTimebin    = timebin;
    mAdc        = adc;
    mEnergy     = e;
    mCrtSlotCh = (crate<<12) + (slot<<8) + ch;
}

void StFcsHit::print(Option_t *option) const {cout<< *this <<endl;}

ostream& operator<<(ostream& os, const StFcsHit& v)
{
  return os << Form("StFcsHit: det=%2d id=%3d crt=%1d slot=%2d ch=%2d timebin=%3d adc=%4d E=%6.2f",
		    v.detector(),v.id(),v.crate(),v.slot(),v.channel(),
		    v.timebin(),v.adc(),v.energy());
}
