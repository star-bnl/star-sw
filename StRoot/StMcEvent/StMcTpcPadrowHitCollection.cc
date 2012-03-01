/***************************************************************************
 *
 * $Id: StMcTpcPadrowHitCollection.cc,v 2.3 2012/03/01 16:48:30 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Tpc Padrow Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcTpcPadrowHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:30  perev
 * method Browse() added
 *
 * Revision 2.2  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2000/05/11 14:27:23  calderon
 * use clear() in destructors to reduce size of containers
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcTpcPadrowHitCollection.hh"
#include "StMcTpcHit.hh"

static const char rcsid[] = "$Id: StMcTpcPadrowHitCollection.cc,v 2.3 2012/03/01 16:48:30 perev Exp $";

ClassImp(StMcTpcPadrowHitCollection)

//_____________________________________________________________________________
StMcTpcPadrowHitCollection::StMcTpcPadrowHitCollection() { /* noop */ }

//_____________________________________________________________________________
StMcTpcPadrowHitCollection::~StMcTpcPadrowHitCollection()
{
  Clear();
}

//_____________________________________________________________________________
const StSPtrVecMcTpcHit&
StMcTpcPadrowHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcTpcHit&
StMcTpcPadrowHitCollection::hits() { return mHits; }
//_____________________________________________________________________________
void StMcTpcPadrowHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcTpcPadrowHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}
