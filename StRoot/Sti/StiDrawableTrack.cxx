#include "StiIOBroker.h"
#include "StiDrawableTrack.h"
#include "StiGui/StiGuiIOBroker.h"

StiDrawableTrack::StiDrawableTrack()
  : Observer()
{
  //mSubject->attach(this);
}

StiDrawableTrack::~StiDrawableTrack()
{
  //  if (mSubject) 
  //  mSubject->detach(this);
}
