/***************************************************************************
 *
 * $Id: StMcCtbHitCollection.cc,v 2.1 2003/02/19 03:29:42 calderon Exp $
 * $Log: StMcCtbHitCollection.cc,v $
 * Revision 2.1  2003/02/19 03:29:42  calderon
 * Introduction of CTB classes to repository.
 *
 * Revision 1.0  2003/03/18 00:00:00  gans
 * Introduction of Ctb classes.  Modified several classes
 * accordingly.
 */
#include "StMcCtbHitCollection.hh"
#include "StMcCtbHit.hh"

static const char rcsid[] = "$Id: StMcCtbHitCollection.cc,v 2.1 2003/02/19 03:29:42 calderon Exp $";

StMcCtbHitCollection::StMcCtbHitCollection()
{
}

StMcCtbHitCollection::~StMcCtbHitCollection()
{
  // StMcCtbHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  for (unsigned int i=0; i<mHits.size(); i++) {
    delete mHits[i];
    mHits[i] = 0;
  }
}
    
bool
StMcCtbHitCollection::addHit(StMcCtbHit* hit)
{
    if (hit) {
	mHits.push_back(hit);
        return true;
    }
    else
        return false;
}

unsigned long
StMcCtbHitCollection::numberOfHits() const
{
    return mHits.size();
}

const StSPtrVecMcCtbHit&
StMcCtbHitCollection::hits() const { return mHits; }

StSPtrVecMcCtbHit&
StMcCtbHitCollection::hits() { return mHits; }
