/***************************************************************************
 *
 * $Id: StEmcRawHit.cxx,v 2.2 2000/05/22 19:21:54 akio Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcRawHit.cxx,v $
 * Revision 2.2  2000/05/22 19:21:54  akio
 * Bug fix, add delta into EMcPoint, wider bits for Eta in RawHit
 *
 * Revision 2.1  2000/02/23 17:34:12  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcRawHit.h"

static const char rcsid[] = "$Id: StEmcRawHit.cxx,v 2.2 2000/05/22 19:21:54 akio Exp $";

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
    mId  = UInt_t(d - kBarrelEmcTowerId) << 20;
    mId += m << 13;
    mId += e << 4;
    mId += s;
}

StDetectorId
StEmcRawHit::detector() const
{ 
    return static_cast<StDetectorId>(bits(20, 4)+kBarrelEmcTowerId); // bits 20-24
}

UInt_t
StEmcRawHit::module() const {return bits(13, 7);}   // bits 13-19

UInt_t
StEmcRawHit::eta() const {return bits( 4, 9);}  // bits 4-12

Int_t
StEmcRawHit::sub() const {
  Int_t sub;
  switch(detector()){
  case kBarrelSmdEtaStripId:                         // irrelevant case return negative
  case kEndcapSmdEtaStripId: case kEndcapSmdPhiStripId: 
    sub = -1; break;
  default:
    sub = bits(0, 4);                         // bits 0-3
  }   
  return sub;
}

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






