/***************************************************************************
 *
 * $Id: StEstPrimaryTrack.cxx,v 2.3 2004/07/15 16:36:24 ullrich Exp $
 *
 * Author: Thomas Ullrich, Mar 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEstPrimaryTrack.cxx,v $
 * Revision 2.3  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.2  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.1  2002/04/20 02:41:42  jeromel
 * Missing files added 9SVT changes)
 *
 **************************************************************************/
#include "TClass.h"
#include "StEstPrimaryTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StPrimaryVertex.h"

ClassImp(StEstPrimaryTrack)

static const char rcsid[] = "$Id: StEstPrimaryTrack.cxx,v 2.3 2004/07/15 16:36:24 ullrich Exp $";

StEstPrimaryTrack::StEstPrimaryTrack() {/* noop */}

StEstPrimaryTrack::StEstPrimaryTrack(const dst_track_st& track) :
    StPrimaryTrack(track) {/* noop */}

StEstPrimaryTrack::StEstPrimaryTrack(const StEstPrimaryTrack& track) :
    StPrimaryTrack(track)
{
}

StEstPrimaryTrack&
StEstPrimaryTrack::operator=(const StEstPrimaryTrack& track)
{
    if (this != &track) {
        static_cast<StTrack&>(*this) = track;
    }
    return *this;
}

StEstPrimaryTrack::~StEstPrimaryTrack() {/* noop */}

StTrackType
StEstPrimaryTrack::type() const { return estPrimary; }
