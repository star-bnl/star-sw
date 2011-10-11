/***************************************************************************
 *
 * $Id: StMcMtdHitCollection.cc,v 2.1 2011/10/11 16:22:39 perev Exp $
 * $Log: StMcMtdHitCollection.cc,v $
 * Revision 2.1  2011/10/11 16:22:39  perev
 * Add Mtd
 *
 *
 *
 */
#include "StMcMtdHitCollection.hh"
#include "StMcMtdHit.hh"
#include <cassert>

static const char rcsid[] = "$Id: StMcMtdHitCollection.cc,v 2.1 2011/10/11 16:22:39 perev Exp $";

ClassImp(StMcMtdHitCollection);
//_____________________________________________________________________________
StMcMtdHitCollection::StMcMtdHitCollection()
{
}

//_____________________________________________________________________________
StMcMtdHitCollection::~StMcMtdHitCollection()
{
  Clear();
}
    
//_____________________________________________________________________________
bool StMcMtdHitCollection::addHit(StMcMtdHit* hit)
{
  assert(hit && "Zero hit pointer added");
  assert(dynamic_cast<StMcMtdHit*>(hit));
  mHits.push_back(hit);
  return true;
}

//_____________________________________________________________________________
unsigned long StMcMtdHitCollection::numberOfHits() const
{
    return mHits.size();
}

//_____________________________________________________________________________
const StSPtrVecMcMtdHit& StMcMtdHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcMtdHit& StMcMtdHitCollection::hits() { return mHits; }


//_____________________________________________________________________________
void StMcMtdHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
