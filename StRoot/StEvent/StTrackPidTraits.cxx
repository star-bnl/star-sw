/***************************************************************************
 *
 * $Id: StTrackPidTraits.cxx,v 1.2 1999/04/30 13:16:30 fisyak Exp $
 *
 * Author: Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.cxx,v $
 * Revision 1.2  1999/04/30 13:16:30  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.2  1999/04/30 13:16:30  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.1  1999/04/28 22:27:37  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/08 14:56:28  ullrich
 * Initial Revision
 *
 * Revision 2.3  1999/11/29 16:53:24  ullrich
 *
#include "StGlobalTrack.h"
#include "StTpcDedxPid.h"
 * Revision 2.2  1999/11/15 18:48:25  ullrich
#include "tables/dst_dedx.h"
 *
StTrackPidTraits::StTrackPidTraits(StGlobalTrack* track)
{
    mTpcDedxPid = new StTpcDedxPid(&(*track));
    // mSvtDedxPid = new StSvtDedxPid(track);
}

StTrackPidTraits::~StTrackPidTraits()
{
    delete mTpcDedxPid;
    // delete mSvtDedxPid
} 
    mDetectorId(det), mMethod(meth) { /* noop */ }

StTrackPidTraits::method() const { return mMethod; }
	break;
    default:
	return kUndefinedMethodId;
	break;
    }
}

Short_t
StTrackPidTraits::detector() const { return mDetectorId; }
