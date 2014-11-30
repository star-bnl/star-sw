/***************************************************************************
 *
 * $Id: StTrackMassFit.cxx,v 1.1.1.1 2013/07/23 14:13:30 fisyak Exp $
 *
 * Author: Yuri Fisyak, April 5, 2013
***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrackMassFit.cxx,v $
 * Revision 1.1.1.1  2013/07/23 14:13:30  fisyak
 *
 *
 * Revision 2.3  2013/07/16 14:29:04  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.1  2013/04/05 15:08:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTrackMassFit.h"

ClassImp(StTrackMassFit)

static const char rcsid[] = "$Id: StTrackMassFit.cxx,v 1.1.1.1 2013/07/23 14:13:30 fisyak Exp $";
//________________________________________________________________________________
StTrackMassFit::StTrackMassFit(const StTrackMassFit& track) : StTrack(track) {
    mKFParticle=0;
    if (track.mKFParticle) mKFParticle = new KFParticle(*(track.mKFParticle));
}
//________________________________________________________________________________
StTrackMassFit& StTrackMassFit::operator=(const StTrackMassFit& track)
{
    if (this != &track) {
        delete mKFParticle; mKFParticle=0;
        static_cast<StTrack&>(*this) = track;
        if (track.mKFParticle) mKFParticle = new KFParticle(*(track.mKFParticle));
    }
    return *this;
}
//________________________________________________________________________________
std::ostream&  operator<<(std::ostream& os,  const StTrackMassFit& track) {
    os << *((StTrack *) &track);
    const KFParticle* kfParticle    = track.kfParticle();
    if (kfParticle) os << " " << *kfParticle;
    return os;
}
