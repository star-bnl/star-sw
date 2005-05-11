/***************************************************************************
 *
 * $Id: StMcIstLayerHitCollection.cc,v 2.2 2005/05/11 20:54:29 calderon Exp $
 *
 * Author: Fabrice Retiere/Kai Schweda, Aug 2003
 ***************************************************************************
 *
 * Description: Monte Carlo Ist Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcIstLayerHitCollection.cc,v $
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
#include "StMcIstLayerHitCollection.hh"
#include "StMcIstHit.hh"
static const char rcsid[] = "$Id: StMcIstLayerHitCollection.cc,v 2.2 2005/05/11 20:54:29 calderon Exp $";

ClassImp(StMcIstLayerHitCollection)

StMcIstLayerHitCollection::StMcIstLayerHitCollection() { /* noop */ }

StMcIstLayerHitCollection::~StMcIstLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
  }
}

const StSPtrVecMcIstHit&
StMcIstLayerHitCollection::hits() const { return mHits; }

StSPtrVecMcIstHit&
StMcIstLayerHitCollection::hits() { return mHits; }

unsigned long StMcIstLayerHitCollection::numberOfHits() const
{
  return mHits.size();
}
