/***************************************************************************
 *
 * $Id: StHit.cc,v 1.1 1999/01/15 20:39:49 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHit.cc,v $
 * Revision 1.1  1999/01/15 20:39:49  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:53:45  wenaus
#include "StHit.hh"
#include "StGlobalTrack.hh"
#include "StTrackCollection.hh"
#include "StEvent/StTrackCollection.hh"

static const char rcsid[] = "$Id: StHit.cc,v 1.1 1999/01/15 20:39:49 wenaus Exp $";

StHit::StHit()
{
    mCharge = 0;
    mTrackRefCount = 0;
}

StHit::StHit(const StThreeVector<float>& p,
	     const StThreeVector<float>& e,
  mPositionError.setY(pt->pos_err[0]/1048576 + (pt->pos_err[1]%1024)*1024);
  mPositionError.setZ(pt->pos_err[1]/1024);
}

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
    os << "Error: " << h.positionError() << endl;
    os << "Charge: " << h.charge() << endl;
    os << "RefCount: " << h.trackReferenceCount() << endl;
    return os;
}
