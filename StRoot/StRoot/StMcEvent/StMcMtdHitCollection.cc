/***************************************************************************
 *
 * $Id: StMcMtdHitCollection.cc,v 2.2 2012/03/01 16:48:29 perev Exp $
 * $Log: StMcMtdHitCollection.cc,v $
 * Revision 2.2  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.1  2011/10/11 16:22:39  perev
 * Add Mtd
 *
 *
 *
 */
#include "TBrowser.h"
#include "StMcMtdHitCollection.hh"
#include "StMcMtdHit.hh"
#include <cassert>

static const char rcsid[] = "$Id: StMcMtdHitCollection.cc,v 2.2 2012/03/01 16:48:29 perev Exp $";

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
  mHits.push_back(hit);
  return true;
}
//_____________________________________________________________________________
void StMcMtdHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcMtdHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
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

//______________________________________________________________________________
//______________________________________________________________________________
