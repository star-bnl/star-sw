/***************************************************************************
 *
 * $Id: StEstPrimaryTrack.cxx,v 2.1 2002/04/20 02:41:42 jeromel Exp $
 *
 * Author: Thomas Ullrich, Mar 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEstPrimaryTrack.cxx,v $
 * Revision 2.1  2002/04/20 02:41:42  jeromel
 * Missing files added 9SVT changes)
 *
 **************************************************************************/
#include "TClass.h"
#include "StEstPrimaryTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StPrimaryVertex.h"

ClassImp(StEstPrimaryTrack)

static const char rcsid[] = "$Id: StEstPrimaryTrack.cxx,v 2.1 2002/04/20 02:41:42 jeromel Exp $";

StEstPrimaryTrack::StEstPrimaryTrack() : mVertex(0) {/* noop */}

StEstPrimaryTrack::StEstPrimaryTrack(const dst_track_st& track) :
    StPrimaryTrack(track), mVertex(0) {/* noop */}

StEstPrimaryTrack::StEstPrimaryTrack(const StEstPrimaryTrack& track) :
    StPrimaryTrack(track)
{
    mVertex = track.mVertex;
}

StEstPrimaryTrack&
StEstPrimaryTrack::operator=(const StEstPrimaryTrack& track)
{
    if (this != &track) {
        static_cast<StTrack&>(*this) = track;
        mVertex = track.mVertex;
    }
    return *this;
}

StEstPrimaryTrack::~StEstPrimaryTrack() {/* noop */}

StObject*
StEstPrimaryTrack::clone() const { return new StEstPrimaryTrack(*this); }

StTrackType
StEstPrimaryTrack::type() const { return estPrimary; }
