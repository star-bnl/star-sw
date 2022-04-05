/***************************************************************************
 *
 * $Id: StMcFtpcPlaneHitCollection.cc,v 2.2 2012/03/01 16:48:29 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ftpc Plane Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFtpcPlaneHitCollection.cc,v $
 * Revision 2.2  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.1  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcFtpcPlaneHitCollection.hh"
#include "StMcFtpcHit.hh"
static const char rcsid[] = "$Id: StMcFtpcPlaneHitCollection.cc,v 2.2 2012/03/01 16:48:29 perev Exp $";

ClassImp(StMcFtpcPlaneHitCollection)

//_____________________________________________________________________________
StMcFtpcPlaneHitCollection::StMcFtpcPlaneHitCollection() { /* noop */ }
StMcFtpcPlaneHitCollection::~StMcFtpcPlaneHitCollection(){ Clear();   }
//_____________________________________________________________________________
void StMcFtpcPlaneHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcFtpcPlaneHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}


//_____________________________________________________________________________
const StSPtrVecMcFtpcHit& StMcFtpcPlaneHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcFtpcHit& StMcFtpcPlaneHitCollection::hits() { return mHits; }

//_____________________________________________________________________________
unsigned long StMcFtpcPlaneHitCollection::numberOfHits() const
{
  return mHits.size();
}
