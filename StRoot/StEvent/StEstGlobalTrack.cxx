/***************************************************************************
 *
 * $Id: StEstGlobalTrack.cxx,v 2.1 2002/04/20 02:41:29 jeromel Exp $
 *
 * Author: Thomas Ullrich, Mar 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEstGlobalTrack.cxx,v $
 * Revision 2.1  2002/04/20 02:41:29  jeromel
 * Missing files added 9SVT changes)
 *
 **************************************************************************/
#include "StEstGlobalTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StVertex.h"

ClassImp(StEstGlobalTrack)

static const char rcsid[] = "$Id: StEstGlobalTrack.cxx,v 2.1 2002/04/20 02:41:29 jeromel Exp $";

StEstGlobalTrack::StEstGlobalTrack() {/* noop */}

StEstGlobalTrack::StEstGlobalTrack(const dst_track_st& track) : StGlobalTrack(track) {/* noop */}

StEstGlobalTrack::StEstGlobalTrack(const StEstGlobalTrack& track) : StGlobalTrack(track) {/* noop */}

StEstGlobalTrack&
StEstGlobalTrack::operator=(const StEstGlobalTrack& track)
{
    if (this != &track)
        static_cast<StTrack&>(*this) = track;
    return *this;
}

StEstGlobalTrack::~StEstGlobalTrack() {/* noop */}

StObject*
StEstGlobalTrack::clone() const { return new StEstGlobalTrack(*this); }

StTrackType
StEstGlobalTrack::type() const { return estGlobal; }
