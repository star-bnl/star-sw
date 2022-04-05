/***************************************************************************
 *
 * $Id: StMcTofHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $
 * $Log: StMcTofHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 */
#include "TBrowser.h"
#include "StMcTofHitCollection.hh"
#include "StMcTofHit.hh"

static const char rcsid[] = "$Id: StMcTofHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $";

ClassImp(StMcTofHitCollection);
//_____________________________________________________________________________
StMcTofHitCollection::StMcTofHitCollection()
{
}

//_____________________________________________________________________________
StMcTofHitCollection::~StMcTofHitCollection()
{
  // StMcTofHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  for (unsigned int i=0; i<mHits.size(); i++) {
    delete mHits[i];
    mHits[i] = 0;
  }
}
    
//_____________________________________________________________________________
bool StMcTofHitCollection::addHit(StMcTofHit* hit)
{
    if (!hit) return false;
    mHits.push_back(hit);
    return true;
}

//_____________________________________________________________________________
unsigned long StMcTofHitCollection::numberOfHits() const
{
    return mHits.size();
}

//_____________________________________________________________________________
const StSPtrVecMcTofHit& StMcTofHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcTofHit& StMcTofHitCollection::hits() { return mHits; }
//_____________________________________________________________________________
void StMcTofHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcTofHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}
