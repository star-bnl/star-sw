/***************************************************************************
 *
 * $Id: StHit.cxx,v 1.1 1999/01/30 03:58:06 fisyak Exp $
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
 * $Log: StHit.cxx,v $
 * Revision 1.1  1999/01/30 03:58:06  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/01/30 23:03:12  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:45  wenaus
 * version with constructors for table-based loading
#include "StGlobalTrack.h"
#include "StTrackCollection.h"
 * Added member mFlag and access member flag() and setFlag().
static const Char_t rcsid[] = "$Id: StHit.cxx,v 1.1 1999/01/30 03:58:06 fisyak Exp $";
#include "StGlobalTrack.h"
#ifdef __ROOT__
 *
static const Char_t rcsid[] = "$Id: StHit.cxx,v 1.1 1999/01/30 03:58:06 fisyak Exp $";
#endif
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:17  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StTrack.h"
#include "StTrackNode.h"
#include "StTrackDetectorInfo.h"

	     const StThreeVectorF& e,
	     Float_t q, UChar_t c)
StHit::StHit(dst_point_st* pt)
{
  /*
    "struct dst_point { 
    Long_t hw_position; 
    Long_t position[2]; 
    Long_t pos_err[2]; 
    Long_t charge; 
    Long_t id_track; 
  */
}

    : mPosition(p), mPositionError(e), mCharge(q), mTrackRefCount(c)
{
    : StMeasuredPoint(p), mPositionError(e), mHardwarePosition(hp),
      mCharge(q), mTrackRefCount(c)
    mHardwarePosition = 0;
Int_t StHit::operator==(const StHit& h) const
    
{
           h.mCharge   == mCharge;
StHit::~StHit() { /* noop */ }

Int_t StHit::operator!=(const StHit& h) const
   
    return h.mPosition == mPosition &&
        h.mPositionError == h.mPositionError &&
        h.mCharge           == mCharge &&
void StHit::setPosition(const StThreeVectorF& val) { mPosition = val; }

void StHit::setPositionError(const StThreeVectorF& val) { mPositionError = val; }
    
void StHit::setCharge(Float_t val) { mCharge = val; }
        return kTpcSsdId;
void StHit::setTrackReferenceCount(UChar_t val) { mTrackRefCount = val; }
    
    os << "Error: " << h.positionError() << endl;
    os << "Charge: " << h.charge() << endl;
    os << "Position: " << h.position() << endl;
    os << "Error:    " << h.positionError() << endl;
    os << "Charge:   " << h.charge() << endl;
  cout << *this << endl;

//______________________________________________________________________________
        const StTrackNode *node = nodes[i];
        unsigned int ntracks = node->entries(type);
        for (unsigned int k=0; k<ntracks; k++) {
            const StTrack *track = node->track(type, k);
            hvec = track->detectorInfo()->hits(id);
            for (unsigned int j=0; j<hvec.size(); j++)
                if (hvec[j] == this) vec.push_back(const_cast<StTrack*>(track));
        }
    }
    return vec;
}

