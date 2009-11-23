/***************************************************************************
 *
 * $Id: StGlobalTrack.cxx,v 2.6 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.cxx,v $
 * Revision 2.6  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.5  2007/03/20 20:56:19  perev
 * LeakFix StDcaGeometry was not deleted at all
 *
 * Revision 2.4  2006/05/24 17:28:19  ullrich
 * Added track-at-DCA geometry.
 *
 * Revision 2.3  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.2  2001/03/24 03:34:47  perev
 * clone() -> clone() const
 *
 * Revision 2.1  1999/10/28 22:25:36  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:10  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StGlobalTrack.h"
#include "StVertex.h"
#include "StDcaGeometry.h"

ClassImp(StGlobalTrack)

static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 2.6 2009/11/23 16:34:06 fisyak Exp $";

StGlobalTrack::StGlobalTrack() {mDcaGeometry = 0;}

StGlobalTrack::StGlobalTrack(const StGlobalTrack& track) : StTrack(track)
{
    mDcaGeometry=0;
    if (track.mDcaGeometry) mDcaGeometry = new StDcaGeometry(*(track.mDcaGeometry));
}

StGlobalTrack& StGlobalTrack::operator=(const StGlobalTrack& track)
{
    if (this != &track) {
        delete mDcaGeometry; mDcaGeometry=0;
	static_cast<StTrack&>(*this) = track;
        if (track.mDcaGeometry) mDcaGeometry = new StDcaGeometry(*(track.mDcaGeometry));
    }
    return *this;
}

StGlobalTrack::~StGlobalTrack() {delete mDcaGeometry;}

StTrackType StGlobalTrack::type() const { return global; }

const StVertex* StGlobalTrack::vertex() const { return 0; }

const StDcaGeometry* StGlobalTrack::dcaGeometry() const {return mDcaGeometry;}

StDcaGeometry* StGlobalTrack::dcaGeometry() {return mDcaGeometry;}

void StGlobalTrack::setDcaGeometry(StDcaGeometry *dca) {mDcaGeometry=dca;}
