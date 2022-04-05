#include <assert.h>
#include <stdio.h>

#include <StEvent/StEvent.h>
#include "StEventToolkit.h"

//______________________________________________________________________________
StEventToolkit* StEventToolkit::instance()
{
static StEventToolkit* myInst = 0;
  if (!myInst) myInst = new StEventToolkit;
  return myInst;
}
//______________________________________________________________________________
const StSPtrVecTrackNode* StEventToolkit::getTrackContainer()
{
  return &(mEvent->trackNodes());
}
