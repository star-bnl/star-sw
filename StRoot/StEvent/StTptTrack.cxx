/***************************************************************************
 *
 * $Id: StTptTrack.cxx,v 2.3 2004/07/15 16:36:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTptTrack.cxx,v $
 * Revision 2.3  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.2  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/08/17 00:10:34  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTptTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StVertex.h"

ClassImp(StTptTrack)

static const char rcsid[] = "$Id: StTptTrack.cxx,v 2.3 2004/07/15 16:36:25 ullrich Exp $";

StTptTrack::StTptTrack() {/* noop */}

StTptTrack::StTptTrack(const dst_track_st& track) : StTrack(track) {/* noop */}

StTptTrack::StTptTrack(const StTptTrack& track) : StTrack(track) {/* noop */}

StTptTrack&
StTptTrack::operator=(const StTptTrack& track)
{
    if (this != &track)
        static_cast<StTrack&>(*this) = track;
    return *this;
}

StTptTrack::~StTptTrack() {/* noop */}

StTrackType
StTptTrack::type() const { return tpt; }

const StVertex*
StTptTrack::vertex() const { return 0; }
