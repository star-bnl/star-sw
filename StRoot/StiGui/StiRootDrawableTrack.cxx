#include "StiGui/StiRootDrawableTrack.h"

StiRootDrawableTrack::StiRootDrawableTrack()
  : StiDrawableTrack()
{
  _line = new StiRootDrawableLine();
  _hits = new StiRootDrawableHits();
  
  _line->setRemoved(true);
  _hits->setRemoved(true);
  mSubject = StiGuiIOBroker::instance();
  mBroker = StiGuiIOBroker::instance();
  mSubject->attach(this);
}

StiRootDrawableTrack::~StiRootDrawableTrack()
{
  delete _line;
  delete _hits;
}

void StiRootDrawableTrack::reset()
{
  _line->clear();
  _hits->clear();
  _line->setIsAdded(false);
  _hits->setIsAdded(false);
}
