/***************************************************************************
 *
 * $Id: StTofMCSlat.cxx,v 2.1 2001/04/26 01:07:42 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCSlat.cxx,v $
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
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

static const char rcsid[] = "$Id: StTofMCSlat.cxx,v 2.1 2001/04/26 01:07:42 ullrich Exp $";

ClassImp(StTofMCSlat)

StTofMCSlat::StTofMCSlat() { /* nopt */ }

StTofMCSlat::StTofMCSlat(const StructTofMCInfo& MCInfo) :  mTofMCInfo(MCInfo) { /* nopt */ }

StTofMCSlat::~StTofMCSlat() { /* noop */ }
    
int
StTofMCSlat::operator==(const StTofMCSlat& MCSlat) const
{
    return (MCSlat.mSlatIndex == mSlatIndex && MCSlat.mTofMCInfo == mTofMCInfo);

}

int
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
                                    << "  MCInfo:  " << slat.mcInfo());
}
