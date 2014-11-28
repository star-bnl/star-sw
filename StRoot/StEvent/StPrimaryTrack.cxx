/***************************************************************************
 *
 * $Id: StPrimaryTrack.cxx,v 1.1.1.1 2013/07/23 14:13:30 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryTrack.cxx,v $
 * Revision 1.1.1.1  2013/07/23 14:13:30  fisyak
 *
 *
 * Revision 2.14  2013/07/16 14:29:04  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.12  2013/04/05 15:11:33  ullrich
 * Changes due to the addition of StTrackMassFit (Yuri)
 *
 * Revision 2.11  2013/01/15 23:21:05  fisyak
 * improve printouts
 *
 * Revision 2.10  2012/10/23 20:18:33  fisyak
 * Add/modify print outs
 *
 * Revision 2.9  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.8  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.7  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.6  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.5  2001/03/24 03:34:52  perev
 * clone() -> clone() const
 *
 * Revision 2.4  2000/04/03 15:12:50  ullrich
 * Fixed bug in vertex(). Always returned 0 instead of
 * data member mVertex.
 *
 * Revision 2.3  1999/11/09 15:44:08  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.2  1999/10/28 22:26:10  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "TClass.h"
#include "TString.h"
#include "StPrimaryTrack.h"
#include "StPrimaryVertex.h"
#include "StTrackGeometry.h"

ClassImp(StPrimaryTrack)

static const char rcsid[] = "$Id: StPrimaryTrack.cxx,v 1.1.1.1 2013/07/23 14:13:30 fisyak Exp $";

//________________________________________________________________________________
std::ostream&  operator<<(std::ostream& os,  const StPrimaryTrack& track) {
    os << *((StTrack *) &track);
    Double_t charge = track.geometry()->charge();
    StThreeVectorD g3 = track.geometry()->momentum(); // p of global track
    os << Form(" q/pT %8.3f eta %8.3f phi %8.3f",
               charge/g3.perp(),g3.pseudoRapidity(),g3.phi());
    os << Form(" pxyz %8.3f%8.3f%8.3f",g3.x(),g3.y(),g3.z());
    Double_t chi2_0 = track.fitTraits().chi2(0); if (chi2_0 > 9999.) chi2_0 = 9999.;
    Double_t chi2_1 = track.fitTraits().chi2(1); if (chi2_1 > 9999.) chi2_1 = 9999.;
    os << Form(" NF %2d chi2 %8.3f/%8.3f", track.fitTraits().numberOfFitPoints(),chi2_0,chi2_1);
#if 0
    if (track.idTruth())
        os << Form(" IdT:%5i Q:%3i", track.idTruth(), track.qaTruth());
#endif
    return os;
}
