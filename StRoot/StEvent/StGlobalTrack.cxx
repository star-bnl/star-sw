/***************************************************************************
 *
 * $Id: StGlobalTrack.cxx,v 2.1 1999/10/28 22:25:36 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.cxx,v $
 * Revision 2.1  1999/10/28 22:25:36  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:10  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StGlobalTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StVertex.h"

ClassImp(StGlobalTrack)

static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 2.1 1999/10/28 22:25:36 ullrich Exp $";

StGlobalTrack::StGlobalTrack() {/* noop */}

StGlobalTrack::StGlobalTrack(const dst_track_st& track) : StTrack(track) {/* noop */}

StGlobalTrack::StGlobalTrack(const StGlobalTrack& track) : StTrack(track) {/* noop */}

StGlobalTrack&
StGlobalTrack::operator=(const StGlobalTrack& track)
{
    if (this != &track)
        static_cast<StTrack&>(*this) = track;
    return *this;
}

StGlobalTrack::~StGlobalTrack() {/* noop */}

StObject*
StGlobalTrack::clone() { return new StGlobalTrack(*this); }

StTrackType
StGlobalTrack::type() const { return global; }

const StVertex*
StGlobalTrack::vertex() const { return 0; }
