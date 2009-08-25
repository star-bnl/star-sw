/***************************************************************************
 *
 * $Id: StMcBTofHitCollection.cc,v 2.2 2009/08/25 20:57:54 fine Exp $
 * $Log: StMcBTofHitCollection.cc,v $
 * Revision 2.2  2009/08/25 20:57:54  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 2.1  2009/07/24 19:08:05  perev
 * Cleanup + Btof added (Geurts)
 *
 *
 */
#include "StMcBTofHitCollection.hh"
#include "StMcBTofHit.hh"
#include <cassert>

static const char rcsid[] = "$Id: StMcBTofHitCollection.cc,v 2.2 2009/08/25 20:57:54 fine Exp $";

ClassImp(StMcBTofHitCollection);
StMcBTofHitCollection::StMcBTofHitCollection()
{
}

StMcBTofHitCollection::~StMcBTofHitCollection()
{
  // StMcTofHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  for (unsigned int i=0; i<mHits.size(); i++) {
    delete mHits[i];
    mHits[i] = 0;
  }
}
    
bool
StMcBTofHitCollection::addHit(StMcBTofHit* hit)
{
  assert(hit && "Zero hit pointer added");
  mHits.push_back(hit);
  return true;
}

unsigned long
StMcBTofHitCollection::numberOfHits() const
{
    return mHits.size();
}

const StSPtrVecMcBTofHit&
StMcBTofHitCollection::hits() const { return mHits; }

StSPtrVecMcBTofHit&
StMcBTofHitCollection::hits() { return mHits; }
