/***************************************************************************
 *
 * $Id: StL3Trigger.cxx,v 2.1 2000/03/29 16:53:07 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL3Trigger.cxx,v $
 * Revision 2.1  2000/03/29 16:53:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StL3Trigger.h"
#include "StTpcHitCollection.h"
#include "StPrimaryVertex.h"

static const char rcsid[] = "$Id: StL3Trigger.cxx,v 2.1 2000/03/29 16:53:07 ullrich Exp $";

ClassImp(StL3Trigger)

StL3Trigger::StL3Trigger()
{
    mL3TpcHits = 0;
}
  
StL3Trigger::~StL3Trigger()
{
    delete mL3TpcHits; mL3TpcHits = 0;
}

StTpcHitCollection*
StL3Trigger::tpcHitCollection() { return mL3TpcHits; }

const StTpcHitCollection*
StL3Trigger::tpcHitCollection() const { return mL3TpcHits; }

StSPtrVecTrackDetectorInfo&
StL3Trigger::trackDetectorInfo() { return mL3TrackDetectorInfo; }

const StSPtrVecTrackDetectorInfo&
StL3Trigger::trackDetectorInfo() const { return mL3TrackDetectorInfo; }

StSPtrVecTrackNode&
StL3Trigger::trackNodes() { return mL3TrackNodes; }

const StSPtrVecTrackNode&
StL3Trigger::trackNodes() const { return mL3TrackNodes; }

UInt_t
StL3Trigger::numberOfPrimaryVertices() const { return mL3PrimaryVertices.size(); }

StPrimaryVertex*
StL3Trigger::primaryVertex(UInt_t i)
{
    if (i < mL3PrimaryVertices.size())
        return mL3PrimaryVertices[i];
    else
        return 0;
}

const StPrimaryVertex*
StL3Trigger::primaryVertex(UInt_t i) const
{
    if (i < mL3PrimaryVertices.size())
        return mL3PrimaryVertices[i];
    else
        return 0;
}

void
StL3Trigger::setTpcHitCollection(StTpcHitCollection* val)
{
    if (mL3TpcHits) delete mL3TpcHits;
    mL3TpcHits = val;
}



void
StL3Trigger::addPrimaryVertex(StPrimaryVertex* vertex)
{
    if (vertex)
        mL3PrimaryVertices.push_back(vertex);
}
