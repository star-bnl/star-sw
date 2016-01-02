#include <limits>

#include "StMixerEvent.h"

StMixerEvent::StMixerEvent() :  mVtx(StThreeVectorF()),
    mBField(std::numeric_limits<float>::quiet_NaN())
{
}
StMixerEvent::StMixerEvent(StMixerEvent *t) : mVtx(t->mVtx), mBField(t->mBField),
					      mTracks(t->mTracks),
					      mEventKaons(t->mEventKaons), mEventPions(t->mEventPions)
{
}
StMixerEvent::StMixerEvent(StThreeVectorF vtx, float b) :  mVtx(StThreeVectorF()),
    mBField(std::numeric_limits<float>::quiet_NaN())
{
    mVtx = vtx;
    mBField = b;

}
void StMixerEvent::addTrack(StMixerTrack t)
{
  mTracks.push_back(t);
}
void StMixerEvent::addPion(int arrayId)
{
  mEventPions.push_back(arrayId);
}
void StMixerEvent::addKaon(int arrayId)
{
  mEventKaons.push_back(arrayId);
}
