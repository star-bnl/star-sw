/***************************************************************************
 *
 * $Id: StTrackPidTraits.cc,v 1.1 1999/04/08 14:56:28 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.cc,v $
 * Revision 1.1  1999/04/08 14:56:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTrackPidTraits.hh"
#include "StGlobalTrack.hh"
#include "StTpcDedxPid.hh"

StTrackPidTraits::StTrackPidTraits(const StGlobalTrack& track)
{
    mTpcDedxPid = new StTpcDedxPid(track);
    // mSvtDedxPid = new StSvtDedxPid(track);
}

StTrackPidTraits::~StTrackPidTraits()
{
    delete mTpcDedxPid;
    // delete mSvtDedxPid
} 
