/***************************************************************************
 *
 * $Id: StMcRichHitCollection.cc,v 2.1 2000/03/06 18:05:21 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, March 2000
 ***************************************************************************
 *
 * Description: Container for StMcRichHit
 *
 ***************************************************************************
 *
 * $Log: StMcRichHitCollection.cc,v $
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#include "StMcRichHitCollection.hh"
#include "StMcRichHit.hh"

static const char rcsid[] = "$Id: StMcRichHitCollection.cc,v 2.1 2000/03/06 18:05:21 calderon Exp $";

StMcRichHitCollection::StMcRichHitCollection()
{
}

StMcRichHitCollection::~StMcRichHitCollection()
{
  // StMcRichHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  for (unsigned int i=0; i<mHits.size(); i++) {
    delete mHits[i];
    mHits[i] = 0;
  }
}
    
bool
StMcRichHitCollection::addHit(StMcRichHit* hit)
{
    if (hit) {
	mHits.push_back(hit);
        return true;
    }
    else
        return false;
}

unsigned long
StMcRichHitCollection::numberOfHits() const
{
    return mHits.size();
}

const StSPtrVecMcRichHit&
StMcRichHitCollection::hits() const { return mHits; }

StSPtrVecMcRichHit&
StMcRichHitCollection::hits() { return mHits; }
