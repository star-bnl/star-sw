/***************************************************************************
 *
 * $Id: StMcRichHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, March 2000
 ***************************************************************************
 *
 * Description: Container for StMcRichHit
 *
 ***************************************************************************
 *
 * $Log: StMcRichHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcRichHitCollection.hh"
#include "StMcRichHit.hh"

static const char rcsid[] = "$Id: StMcRichHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $";
ClassImp(StMcRichHitCollection);
//_____________________________________________________________________________
StMcRichHitCollection::StMcRichHitCollection()
{
}

//_____________________________________________________________________________
StMcRichHitCollection::~StMcRichHitCollection()
{
  // StMcRichHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  Clear();
}
    
//_____________________________________________________________________________
bool StMcRichHitCollection::addHit(StMcRichHit* hit)
{
    if (!hit) return false;
    mHits.push_back(hit);
    return true;
}

//_____________________________________________________________________________
unsigned long StMcRichHitCollection::numberOfHits() const
{
    return mHits.size();
}

//_____________________________________________________________________________
const StSPtrVecMcRichHit& StMcRichHitCollection::hits() const { return mHits; }

StSPtrVecMcRichHit& StMcRichHitCollection::hits() { return mHits; }
//_____________________________________________________________________________
void StMcRichHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcRichHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}
