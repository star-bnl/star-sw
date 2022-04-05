/***************************************************************************
 *
 * $Id: StTofMCSlat.cxx,v 2.5 2003/09/02 17:58:06 perev Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCSlat.cxx,v $
 * Revision 2.5  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.4  2003/05/21 18:23:18  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.3  2001/04/27 21:43:18  ullrich
 * Moved MC info class into separate file.
 *
 * Revision 2.2  2001/04/26 15:19:10  ullrich
 * Added ClassDef and ClassImp macros.
 *
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "Stiostream.h"
#include "StTofMCSlat.h"

static const char rcsid[] = "$Id: StTofMCSlat.cxx,v 2.5 2003/09/02 17:58:06 perev Exp $";

ClassImp(StTofMCSlat)

StTofMCSlat::StTofMCSlat() { /* noop */ }

StTofMCSlat::StTofMCSlat(const StTofMCInfo& MCInfo) :
    mTofMCInfo(MCInfo) { /* noop */ }

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
	    << "MCInfo: " << slat.mcInfo());
}
