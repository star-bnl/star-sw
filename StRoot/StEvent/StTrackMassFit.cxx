/***************************************************************************
 *
 * $Id: StTrackMassFit.cxx,v 2.1 2013/04/05 15:08:41 ullrich Exp $
 *
 * Author: Yuri Fisyak, April 5, 2013
***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrackMassFit.cxx,v $
 * Revision 2.1  2013/04/05 15:08:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTrackMassFit.h"

ClassImp(StTrackMassFit)

static const char rcsid[] = "$Id: StTrackMassFit.cxx,v 2.1 2013/04/05 15:08:41 ullrich Exp $";

StTrackMassFit::StTrackMassFit(const StTrackMassFit& track) : StTrack(track) {
    mKFParticle=0;
    if (track.mKFParticle) mKFParticle = new KFParticle(*(track.mKFParticle));
    mType = track.mType;
}

StTrackMassFit& StTrackMassFit::operator=(const StTrackMassFit& track)
{
    if (this != &track) {
        delete mKFParticle; mKFParticle=0;
        static_cast<StTrack&>(*this) = track;
        if (track.mKFParticle) mKFParticle = new KFParticle(*(track.mKFParticle));
        mType = track.mType;
    }
    return *this;
}

ostream&  operator<<(ostream& os,  const StTrackMassFit& track) {
    os << *((StTrack *) &track);
    const KFParticle* kfParticle    = track.kfParticle();
    if (kfParticle) os << " " << *kfParticle;
    return os;
}
