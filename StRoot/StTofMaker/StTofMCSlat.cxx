/***************************************************************************
 *
 * $Id: StTofMCSlat.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 * Inherited from StTofSlat with MC Information, a structure, added.
 *
 ***************************************************************************
 *
 * $Log: StTofMCSlat.cxx,v $
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 **************************************************************************/
#include "iostream.h"
#include "StTofMCSlat.h"

ostream& operator<<(ostream& os, const StructTofMCInfo& MCInfo)
{
    return (os << "  trkId= " << MCInfo.mTrkId << ", gId=  " << MCInfo.mGId 
               << ", nHits= " << MCInfo.mNHits << ", nPhe= " << MCInfo.mNPhe  
               << ", tof: "   << MCInfo.mTof   << endl); 
}

static const char rcsid[] = "$Id: StTofMCSlat.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $";

ClassImp(StTofMCSlat)


StTofMCSlat::StTofMCSlat() { /* nopt */ }

StTofMCSlat::StTofMCSlat(StructTofMCInfo MCInfo) 
{
  mTofMCInfo = MCInfo;
}

StTofMCSlat::~StTofMCSlat() { /* noop */ }
    
Int_t
StTofMCSlat::operator==(const StTofMCSlat& MCSlat) const
{
    return (MCSlat.mSlatIndex == mSlatIndex && MCSlat.mTofMCInfo == mTofMCInfo);

}


Int_t
StTofMCSlat::operator!=(const StTofMCSlat& MCSlat) const
{
    return !(*this == MCSlat);  // use operator==()
}

ostream&
operator<<(ostream& os, const StTofMCSlat& slat)
{
    return (os << "StTofMCSlat::> " << "  Id=  " << slat.slatIndex()   
                                    << ", adc= " << slat.adc()  
                                    << ", tdc= " << slat.tdc() << endl  
                                    << "  MCInfo:  " << slat.MCInfo());
}
