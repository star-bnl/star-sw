/***************************************************************************
 *
 *
 * Author: Amilkar Quintero, Feb 2015
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Sensor Hit Collection class
 *
 ***************************************************************************/
#include "TBrowser.h"
#include "StMcIstSensorHitCollection.hh"
#include "StMcIstHit.hh"

static const char rcsid[] = "$Id: StMcIstSensorHitCollection.cc,v 2.1 2015/03/12 23:23:43 perev Exp $";
ClassImp(StMcIstSensorHitCollection);
//_____________________________________________________________________________
StMcIstSensorHitCollection::StMcIstSensorHitCollection() { /* noop */ }

//_____________________________________________________________________________
StMcIstSensorHitCollection::~StMcIstSensorHitCollection()
{
  // StMcIstHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  Clear();
}
//_____________________________________________________________________________
const StSPtrVecMcIstHit& StMcIstSensorHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcIstHit& StMcIstSensorHitCollection::hits() { return mHits; }
//_____________________________________________________________________________
void StMcIstSensorHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
/*void StMcIstSensorHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}*/
