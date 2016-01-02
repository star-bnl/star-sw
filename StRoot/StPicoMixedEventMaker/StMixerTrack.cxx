#include <limits>

#include "StMixerTrack.h"
#include "StPicoDstMaker/StPicoTrack.h"

StMixerTrack::StMixerTrack() : mOrigin(StThreeVectorF()), mMom(StThreeVectorF()), mTrackInfo(std::numeric_limits<short>::min())
{
}
StMixerTrack::StMixerTrack(StThreeVectorF const & pVtx, float B,StPicoTrack const& picoTrack, bool isTpcPi, bool isTofPi, bool isTpcK, bool isTofK) :
    mOrigin(picoTrack.helix().origin()), mMom(picoTrack.gMom(pVtx,B)), mTrackInfo(0)
{
    if( picoTrack.charge() == 1 ) mTrackInfo = mTrackInfo | 1;
    //Pi
    if( isTpcPi == true ) mTrackInfo = mTrackInfo | (1 << 1);
    if( isTofPi == true ) mTrackInfo = mTrackInfo | (1 << 2);
    //K
    if( isTpcK == true ) mTrackInfo = mTrackInfo | (1 << 3);
    if( isTofK == true ) mTrackInfo = mTrackInfo | (1 << 4);

}
StMixerTrack::StMixerTrack(StMixerTrack const * t) : mOrigin(t->mOrigin), mMom(t->mMom), mTrackInfo(t->mTrackInfo)
{
}

