/***************************************************************************
 *
 * $Id: StMcIstLayerHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcIstLayerHitCollection.cc,v $
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2005/05/11 20:54:29  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Ist classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMcIstLayerHitCollection.hh"
#include "StMcIstHit.hh"
static const char rcsid[] = "$Id: StMcIstLayerHitCollection.cc,v 2.3 2012/03/01 16:48:29 perev Exp $";

ClassImp(StMcIstLayerHitCollection)

//_____________________________________________________________________________
StMcIstLayerHitCollection::StMcIstLayerHitCollection() { /* noop */ }
//_____________________________________________________________________________
StMcIstLayerHitCollection::~StMcIstLayerHitCollection(){ Clear();   }
//_____________________________________________________________________________
void StMcIstLayerHitCollection::Clear(const char*)
{
  for (int i=0; i<(int)mHits.size(); i++) 
  {
    delete mHits[i]; mHits[i] = 0;
  }
  mHits.clear();
}
//_____________________________________________________________________________
void StMcIstLayerHitCollection::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (int i=0; i<(int)mHits.size(); i++) {
     TObject *obj = mHits[i]; if (!obj) continue;
     TString ts(obj->GetName()); ts+="#"; ts+=i;
     b->Add(obj,ts.Data());
   }
}


//_____________________________________________________________________________
const StSPtrVecMcIstHit& StMcIstLayerHitCollection::hits() const { return mHits; }

//_____________________________________________________________________________
StSPtrVecMcIstHit& StMcIstLayerHitCollection::hits() { return mHits; }

//_____________________________________________________________________________
unsigned long StMcIstLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
