/***************************************************************************
 *
 * $Id: StMcPixelLayerHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Pixel Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcPixelLayerHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcPixelLayerHitCollection.hh"
#include "StMcPixelHit.hh"
static const char rcsid[] = "$Id: StMcPixelLayerHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $";

ClassImp(StMcPixelLayerHitCollection)

//_____________________________________________________________________________
StMcPixelLayerHitCollection::StMcPixelLayerHitCollection() { /* noop */ }
//_____________________________________________________________________________
StMcPixelLayerHitCollection::~StMcPixelLayerHitCollection(){ Clear();   }

//_____________________________________________________________________________
void StMcPixelLayerHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcPixelLayerHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}

//_____________________________________________________________________________
const StSPtrVecMcPixelHit& StMcPixelLayerHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcPixelHit& StMcPixelLayerHitCollection::hits() { return mHits; }

//_____________________________________________________________________________
unsigned long StMcPixelLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
