/***************************************************************************
 *
 * $Id: StPrimaryTrack.cxx,v 2.1 1999/10/13 19:45:00 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryTrack.cxx,v $
 * Revision 2.1  1999/10/13 19:45:00  ullrich
 * Initial Revision
 *
 * Revision 2.3  1999/11/09 15:44:08  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.2  1999/10/28 22:26:10  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
#include "tables/dst_track.h"
 * Revision 2.1  1999/10/13 19:45:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StPrimaryTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StPrimaryVertex.h"

ClassImp(StPrimaryTrack)

static const char rcsid[] = "$Id: StPrimaryTrack.cxx,v 2.1 1999/10/13 19:45:00 ullrich Exp $";

StPrimaryTrack::StPrimaryTrack() : mVertex(0) {/* noop */}

StPrimaryTrack::StPrimaryTrack(const dst_track_st& track) :
    StTrack(track), mVertex(0) {/* noop */}

StPrimaryTrack::StPrimaryTrack(const StPrimaryTrack& track) :
    StTrack(track)
{
    mVertex = track.mVertex;
}

StPrimaryTrack&
StPrimaryTrack::operator=(const StPrimaryTrack& track)
{
    if (this != &track) {
        static_cast<StTrack&>(*this) = track;
        mVertex = track.mVertex;
    }
StPrimaryTrack::~StPrimaryTrack()
{
}

StPrimaryTrack::~StPrimaryTrack() {/* noop */}

StObject*
StPrimaryTrack::clone() { return new StPrimaryTrack(*this); }

StTrackType
StPrimaryTrack::type() const { return primary; }

const StVertex*
StPrimaryTrack::vertex() const { return 0; }

void
StPrimaryTrack::setVertex(StVertex* val)
{
    StPrimaryVertex *p = dynamic_cast<StPrimaryVertex*>(val);
    if (p) mVertex = p;
}
