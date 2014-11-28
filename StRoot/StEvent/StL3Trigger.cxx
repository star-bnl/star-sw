/***************************************************************************
 *
 * $Id: StL3Trigger.cxx,v 2.3 2001/08/02 01:27:45 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL3Trigger.cxx,v $
 * Revision 2.3  2001/08/02 01:27:45  ullrich
 * Added event summary and algorithms.
 *
 * Revision 2.2  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/03/29 16:53:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StL3Trigger.h"
#include "StTpcHitCollection.h"
#include "StPrimaryVertex.h"
#include "StL3EventSummary.h"

static const char rcsid[] = "$Id: StL3Trigger.cxx,v 2.3 2001/08/02 01:27:45 ullrich Exp $";

ClassImp(StL3Trigger)

StL3Trigger::StL3Trigger()
{
    mL3TpcHits = 0;
    mL3EventSummary = 0;
}
  
StL3Trigger::~StL3Trigger()
{
    delete mL3TpcHits; mL3TpcHits = 0;
    delete mL3EventSummary; mL3EventSummary = 0;
}

StL3EventSummary*
StL3Trigger::l3EventSummary() { return mL3EventSummary; }

const StL3EventSummary*
StL3Trigger::l3EventSummary() const { return mL3EventSummary; }

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

unsigned int
StL3Trigger::numberOfPrimaryVertices() const { return mL3PrimaryVertices.size(); }

StPrimaryVertex*
StL3Trigger::primaryVertex(unsigned int i)
{
    if (i < mL3PrimaryVertices.size())
        return mL3PrimaryVertices[i];
    else
        return 0;
}

const StPrimaryVertex*
StL3Trigger::primaryVertex(unsigned int i) const
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
StL3Trigger::setL3EventSummary(StL3EventSummary* evsum)
{
    if (mL3EventSummary) delete mL3EventSummary;
    mL3EventSummary = evsum;
}


void
StL3Trigger::addPrimaryVertex(StPrimaryVertex* vertex)
{
    if (vertex)
        mL3PrimaryVertices.push_back(vertex);
}
