/***************************************************************************
 *
 * $Id: StMcBTofHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $
 * $Log: StMcBTofHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2009/08/25 20:57:54  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 2.1  2009/07/24 19:08:05  perev
 * Cleanup + Btof added (Geurts)
 *
 *
 */
#include "TBrowser.h"
#include "StMcBTofHitCollection.hh"
#include "StMcBTofHit.hh"
#include <cassert>

static const char rcsid[] = "$Id: StMcBTofHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $";

//_____________________________________________________________________________
ClassImp(StMcBTofHitCollection);
//_____________________________________________________________________________
StMcBTofHitCollection::StMcBTofHitCollection()
{
}

//_____________________________________________________________________________
StMcBTofHitCollection::~StMcBTofHitCollection()
{
  // StMcTofHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  Clear();
}
    
//_____________________________________________________________________________
bool StMcBTofHitCollection::addHit(StMcBTofHit* hit)
{
  assert(hit && "Zero hit pointer added");
  mHits.push_back(hit);
  return true;
}

//_____________________________________________________________________________
unsigned long StMcBTofHitCollection::numberOfHits() const
{
    return mHits.size();
}

//_____________________________________________________________________________
const StSPtrVecMcBTofHit& StMcBTofHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcBTofHit& StMcBTofHitCollection::hits() { return mHits; }
//_____________________________________________________________________________
void StMcBTofHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcBTofHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}
