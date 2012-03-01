/***************************************************************************
 *
 * $Id: StMcCtbHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $
 * $Log: StMcCtbHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2005/01/27 23:40:46  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/02/19 03:29:42  calderon
 * Introduction of CTB classes to repository.
 *
 * Revision 1.0  2003/03/18 00:00:00  gans
 * Introduction of Ctb classes.  Modified several classes
 * accordingly.
 */
#include "TBrowser.h"
#include "StMcCtbHitCollection.hh"
#include "StMcCtbHit.hh"

static const char rcsid[] = "$Id: StMcCtbHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $";
ClassImp(StMcCtbHitCollection)
//_____________________________________________________________________________
StMcCtbHitCollection::StMcCtbHitCollection() : StObject()
{
}

//_____________________________________________________________________________
StMcCtbHitCollection::~StMcCtbHitCollection()
{
  // StMcCtbHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  Clear();
}
    
//_____________________________________________________________________________
unsigned long
StMcCtbHitCollection::numberOfHits() const
{
    return mHits.size();
}

//_____________________________________________________________________________
const StSPtrVecMcCtbHit&
StMcCtbHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcCtbHit& StMcCtbHitCollection::hits() { return mHits; }
//_____________________________________________________________________________
bool StMcCtbHitCollection::addHit(StMcCtbHit* hit)
{
  assert(hit && "Zero hit pointer added");
  mHits.push_back(hit);
  return true;
}
//_____________________________________________________________________________
void StMcCtbHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcCtbHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}
