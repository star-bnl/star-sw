/***************************************************************************
 *
 * $Id: StMcFgtLayerHitCollection.cc,v 2.2 2012/03/01 16:48:29 perev Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Fgt Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFgtLayerHitCollection.cc,v $
 * Revision 2.2  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcFgtLayerHitCollection.hh"
#include "StMcFgtHit.hh"
static const char rcsid[] = "$Id: StMcFgtLayerHitCollection.cc,v 2.2 2012/03/01 16:48:29 perev Exp $";

ClassImp(StMcFgtLayerHitCollection)

//_____________________________________________________________________________
StMcFgtLayerHitCollection::StMcFgtLayerHitCollection() { /* noop */ }

//_____________________________________________________________________________
StMcFgtLayerHitCollection::~StMcFgtLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
  Clear();
}

//_____________________________________________________________________________
const StSPtrVecMcFgtHit& StMcFgtLayerHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcFgtHit& StMcFgtLayerHitCollection::hits() { return mHits; }

//_____________________________________________________________________________
unsigned long StMcFgtLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
//_____________________________________________________________________________
void StMcFgtLayerHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcFgtLayerHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}
