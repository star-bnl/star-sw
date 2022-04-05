/***************************************************************************
 *
 * $Id: StEmcRawHit.cxx,v 2.11 2013/01/15 23:21:05 fisyak Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcRawHit.cxx,v $
 * Revision 2.11  2013/01/15 23:21:05  fisyak
 * improve printouts
 *
 * Revision 2.10  2012/10/23 20:18:33  fisyak
 * Add/modify print outs
 *
 * Revision 2.9  2012/09/16 21:33:57  fisyak
 * Add print out
 *
 * Revision 2.8  2007/01/25 02:52:35  ullrich
 * Changes to modEtaSub() and sub() implemented by Adam Kocoloski.
 * sub() now returns 1 instead of -1.
 *
 * Revision 2.7  2004/07/20 17:07:49  perev
 * Pavlinov corrs for TBrowser
 *
 * Revision 2.6  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.5  2001/04/05 04:00:49  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:34:46  perev
 * clone() -> clone() const
 *
 * Revision 2.3  2000/07/28 19:49:28  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.2  2000/05/22 19:21:54  akio
 * Bug fix, add delta into EMcPoint, wider bits for Eta in RawHit
 *
 * Revision 2.1  2000/02/23 17:34:12  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcRawHit.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

static const char rcsid[] = "$Id: StEmcRawHit.cxx,v 2.11 2013/01/15 23:21:05 fisyak Exp $";

ClassImp(StEmcRawHit)

StEmcGeom* StEmcRawHit::mGeom=0;

StEmcRawHit::StEmcRawHit()
{
    mId = 0;
    mAdc = 0;
    mEnergy = 0;
}

StEmcRawHit::StEmcRawHit(StDetectorId d, unsigned int m, unsigned int e, unsigned int s, unsigned int a)
{
    setId(d, m, e, s);
    setAdc(a);
    setEnergy(0.0);
}

StEmcRawHit::StEmcRawHit(StDetectorId d, unsigned int m, unsigned int e, unsigned int s, unsigned int a, float ene)
{
    setId(d, m, e, s);
    setAdc(a);
    setEnergy(ene);
}

StEmcRawHit::StEmcRawHit(const StEmcRawHit& h) : StObject(h){
    mId = h.mId;
    mAdc = h.mAdc;
    mEnergy = h.mEnergy;
}

StEmcRawHit::~StEmcRawHit() {/* noop */}

unsigned int
StEmcRawHit::bits(unsigned int bit, unsigned int nbits) const
{
    return (mId>>bit) & ~(~0UL<<nbits);
}

void
StEmcRawHit::setCalibrationType(unsigned int t){
    if(t<256){
        mId = mId & 0xffffff;
        mId += t << 24;
    }
}

void
StEmcRawHit::setId(StDetectorId d, unsigned int m, unsigned int e, unsigned int s)
{
    mId  = static_cast<unsigned int>(d - kBarrelEmcTowerId) << 20;
    mId += m << 13;
    mId += e << 4;
    mId += s;
}

unsigned int
StEmcRawHit::calibrationType() const {return bits(24, 8);} // bits 24-31

StDetectorId
StEmcRawHit::detector() const
{
    return static_cast<StDetectorId>(bits(20, 4)+kBarrelEmcTowerId); // bits 20-24
}

unsigned int
StEmcRawHit::module() const {return bits(13, 7);}   // bits 13-19

unsigned int
StEmcRawHit::eta() const {return bits( 4, 9);}  // bits 4-12

unsigned int 
StEmcRawHit::softId(int det) const
{
    mGeom = StEmcGeom::instance(det);
    if(mGeom==0) return 0;
    
    int id=0, m=0, e=0, s=0;
    modEtaSub(m,e,s);
    mGeom->getId(m, e, s, id);
    return id;
}

void StEmcRawHit::modEtaSub(int &m, int &e, int &s) const
{
    m = module();
    e = eta();
    s = sub();
}

int
StEmcRawHit::sub() const {
    int sub;
    switch(detector()){
    case kBarrelSmdEtaStripId:                  // sub is irrelevant for these
    case kEndcapSmdUStripId:
    case kEndcapSmdVStripId:
        sub = 1;
        break;
    default:
        sub = bits(0, 4);                         // bits 0-3
    }
    return sub;
}

unsigned int
StEmcRawHit::adc() const {return mAdc;}

float
StEmcRawHit::energy() const {return mEnergy;}

void
StEmcRawHit::setAdc(const unsigned int adc) {mAdc=adc;}

void
StEmcRawHit::setEnergy(const float energy) {mEnergy=energy;}

ostream&  operator<<(ostream& os, const StEmcRawHit& v) {
  os << Form("EmcRawHit d/m %3i/%3i eta/sub  %3i/%3i adc/enery %4i/%f",v.detector(),v.module(), v.eta(), v.sub(), v.adc(), v.energy());
  if (v.detector() == kBarrelEmcTowerId || v.detector() == kEndcapEmcTowerId) {
    if      (v.energy() <=  0.0) os << " Junk";
    else if (v.energy() <=  0.1) os << " Noise";
    else if (v.energy() <=  0.5) os << " MIP";
    else if (v.energy() <=  1.0) os << " Hadron?";
    else if (v.energy() <=  4.0) os << " electron";// lowest HT threshold (4 GeV for Run7)
    else if (v.energy() <= 10.0) os << " Tower";// lowest HT threshold (4 GeV for Run7)
    else if (v.energy() <= 20.0) os << " W";// lowest HT threshold (4 GeV for Run7)
    else if (v.energy() <=100.0) os << " Z";// lowest HT threshold (4 GeV for Run7)
    else                         os << " Hot ?";
  }
  return os;
}
void   StEmcRawHit::Print(Option_t *option) const {cout << *this << endl;}
