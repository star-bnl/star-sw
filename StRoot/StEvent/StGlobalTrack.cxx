/***************************************************************************
 *
 * $Id: StGlobalTrack.cxx,v 2.10 2013/04/05 15:11:33 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.cxx,v $
 * Revision 2.10  2013/04/05 15:11:33  ullrich
 * Changes due to the addition of StTrackMassFit (Yuri)
 *
 * Revision 2.9  2013/01/15 23:21:05  fisyak
 * improve printouts
 *
 * Revision 2.8  2012/06/11 14:40:34  fisyak
 * Adjust format
 *
 * Revision 2.7  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
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
#include "StParticleDefinition.hh"

ClassImp(StGlobalTrack)

static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 2.10 2013/04/05 15:11:33 ullrich Exp $";

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

ostream&  operator<<(ostream& os,  const StGlobalTrack& track) {
    os << *((StTrack *) &track);
    const StDcaGeometry* dca    = track.dcaGeometry();
    if (dca) os << " " << *dca;
    os << Form(" NF %2d chi2 %8.3g gId %2d", track.fitTraits().numberOfFitPoints(),track.fitTraits().chi2(0),track.fitTraits().geantId());
#if 0
    if (track.idTruth())
        os << Form(" IdT: %4i Q: %4i", track.idTruth(), track.qaTruth());
#endif
    return os;
}
