/***************************************************************************
 *
 * $Id: StHit.cc,v 1.4 1999/03/23 21:51:49 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHit.cc,v $
 * Revision 1.4  1999/03/23 21:51:49  ullrich
 * Removed table-based constructor.
 *
 * Revision 1.3  1999/01/30 23:03:12  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:45  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"

static const char rcsid[] = "$Id: StHit.cc,v 1.4 1999/03/23 21:51:49 ullrich Exp $";

StHit::StHit()
{
    mCharge = 0;
    mTrackRefCount = 0;
}

StHit::StHit(const StThreeVector<float>& p,
	     const StThreeVector<float>& e,
	     float q, unsigned char c)
    : mPosition(p), mPositionError(e), mCharge(q), mTrackRefCount(c)
{ /* noop */ }

StHit::~StHit() { /* noop */ }
    
int StHit::operator==(const StHit& h) const
{
    return h.mPosition == mPosition &&
           h.mCharge   == mCharge;
}

int StHit::operator!=(const StHit& h) const
{
    return !(*this == h);  // use operator==()
}

void StHit::setPosition(const StThreeVector<float>& val) { mPosition = val; }

void StHit::setPositionError(const StThreeVector<float>& val) { mPositionError = val; }

void StHit::setCharge(float val) { mCharge = val; }

void StHit::setTrackReferenceCount(unsigned char val) { mTrackRefCount = val; }
    
ostream& operator<<(ostream& os, const StHit& h)
{
    os << "Position: " << h.position() << endl;
    os << "Error:    " << h.positionError() << endl;
    os << "Charge:   " << h.charge() << endl;
    os << "RefCount: " << h.trackReferenceCount() << endl;
    return os;
}
