/***************************************************************************
 *
 * $Id: StMcTofHitCollection.cc,v 2.1 2003/08/20 18:50:21 calderon Exp $
 * $Log: StMcTofHitCollection.cc,v $
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 */
#include "StMcTofHitCollection.hh"
#include "StMcTofHit.hh"

static const char rcsid[] = "$Id: StMcTofHitCollection.cc,v 2.1 2003/08/20 18:50:21 calderon Exp $";

StMcTofHitCollection::StMcTofHitCollection()
{
}

StMcTofHitCollection::~StMcTofHitCollection()
{
  // StMcTofHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  for (unsigned int i=0; i<mHits.size(); i++) {
    delete mHits[i];
    mHits[i] = 0;
  }
}
    
bool
StMcTofHitCollection::addHit(StMcTofHit* hit)
{
    if (hit) {
	mHits.push_back(hit);
        return true;
    }
    else
        return false;
}

unsigned long
StMcTofHitCollection::numberOfHits() const
{
    return mHits.size();
}

const StSPtrVecMcTofHit&
StMcTofHitCollection::hits() const { return mHits; }

StSPtrVecMcTofHit&
StMcTofHitCollection::hits() { return mHits; }
