/***************************************************************************
 *
 * $Id: StTptTrack.cxx,v 2.1 2000/08/17 00:10:34 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTptTrack.cxx,v $
 * Revision 2.1  2000/08/17 00:10:34  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTptTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StVertex.h"

ClassImp(StTptTrack)

static const char rcsid[] = "$Id: StTptTrack.cxx,v 2.1 2000/08/17 00:10:34 ullrich Exp $";

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

StObject*
StTptTrack::clone() { return new StTptTrack(*this); }

StTrackType
StTptTrack::type() const { return tpt; }

const StVertex*
StTptTrack::vertex() const { return 0; }
