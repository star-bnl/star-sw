/***************************************************************************
 *
 * $Id: StEmcRawHit.cxx,v 2.1 2000/02/23 17:34:12 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcRawHit.cxx,v $
 * Revision 2.1  2000/02/23 17:34:12  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcRawHit.h"

static const char rcsid[] = "$Id: StEmcRawHit.cxx,v 2.1 2000/02/23 17:34:12 ullrich Exp $";

ClassImp(StEmcRawHit)

StEmcRawHit::StEmcRawHit()
{
    mId = 0;
    mAdc = 0;        
    mEnergy = 0;
}

StEmcRawHit::StEmcRawHit(StDetectorId d, UInt_t m, UInt_t e, UInt_t s, UInt_t a)
{
    setId(d, m, e, s);
    setAdc(a);
    setEnergy(0.0);
}

StEmcRawHit::StEmcRawHit(StDetectorId d, UInt_t m, UInt_t e, UInt_t s, UInt_t a, Float_t ene)
{
    setId(d, m, e, s);
    setAdc(a);
    setEnergy(ene);
}

StEmcRawHit::~StEmcRawHit() {/* noop */}

UInt_t
StEmcRawHit::bits(UInt_t bit, UInt_t nbits) const
{
    return (mId>>bit) & ~(~0UL<<nbits);
}

void
StEmcRawHit::setId(StDetectorId d, UInt_t m, UInt_t e, UInt_t s)
{
    mId  = UInt_t(d - kBarrelEmcTowerId) << 19;
    mId += m << 12;
    mId += e << 4;
    mId += s;
}

StDetectorId
StEmcRawHit::detector() const
{ 
    return static_cast<StDetectorId>(bits(19, 4)+kBarrelEmcTowerId); // bits 19-23
}

UInt_t
StEmcRawHit::module() const {return bits(12, 7);}   // bits 12-18

UInt_t
StEmcRawHit::eta() const {return bits( 4, 8);}   // bits 4-11

UInt_t
StEmcRawHit::sub() const {return bits( 0, 4);}   // bits 0-3

UInt_t
StEmcRawHit::adc() const {return mAdc;}   

Float_t
StEmcRawHit::energy() const {return mEnergy;}

void
StEmcRawHit::setAdc(const UInt_t adc) {mAdc=adc;}   

void
StEmcRawHit::setEnergy(const Float_t energy) {mEnergy=energy;}   

StObject*
StEmcRawHit::clone() { return new StEmcRawHit(*this); }

