/***************************************************************************
 *
 * $Id: StRichSimpleMCHit.cxx,v 1.2 2000/06/16 02:07:04 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of MCHit definition
 *
 ***************************************************************************
 * $Log: StRichSimpleMCHit.cxx,v $
 * Revision 1.2  2000/06/16 02:07:04  lasiuk
 * explicit pathnames for different Maker files
 *
 * Revision 1.1  2000/05/23 16:54:22  lasiuk
 * Initial Revision
 *
 ***************************************************************************/
#include "StRichSimpleMCHit.h"
#ifdef __ROOT__
#include "StEvent/StRichMCHit.h"
#endif

StRichSimpleMCHit::StRichSimpleMCHit() {/* nopt */}

StRichSimpleMCHit::StRichSimpleMCHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx)
    : StRichSimpleHit(xl, dx)
{
    // This is used for off-line
}

StRichSimpleMCHit::StRichSimpleMCHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx, const StRichID& info)
    : StRichSimpleHit(xl, dx), mMCInfo(info)
{
    // This is used for off-line
}

#ifdef __ROOT__
StRichSimpleMCHit::StRichSimpleMCHit(const StRichMCHit*)
{
    cout << "StRichSimpleMCHit::StRichSimpleMCHit(const StRichMCHit*)" << endl;
}
#endif

StRichSimpleMCHit::~StRichSimpleMCHit() {/* nopt */}

ostream& operator<<(ostream& os, const StRichSimpleMCHit& hit)
{
    return (os << "StRichSimpleMCHit::> " << hit.internal() << ", q= "
	                                  << hit.charge()   << ", (#"
	                                  << hit.clusterNumber() << ')'
	                                  << hit.getMCInfo());
}
