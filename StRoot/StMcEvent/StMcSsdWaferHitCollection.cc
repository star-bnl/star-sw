/***************************************************************************
 *
 * $Id: StMcSsdWaferHitCollection.cc,v 2.1 2005/11/22 21:44:52 fisyak Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ssd Wafer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSsdWaferHitCollection.cc,v $
 * Revision 2.1  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.4  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSsdWaferHitCollection.
 *
 * Revision 2.2  2000/03/06 18:05:23  calderon
 * 1) Modified SSD Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcSsdWaferHitCollection.hh"
#include "StMcSsdHit.hh"

static const char rcsid[] = "$Id: StMcSsdWaferHitCollection.cc,v 2.1 2005/11/22 21:44:52 fisyak Exp $";
ClassImp(StMcSsdWaferHitCollection);
StMcSsdWaferHitCollection::StMcSsdWaferHitCollection() { /* noop */ }

StMcSsdWaferHitCollection::~StMcSsdWaferHitCollection()
{
  // StMcSsdHit provides its own new/delete operator, and
  // mHits is a polymorphic container, so we need to do this.
  for (unsigned int i=0; i<mHits.size(); i++) {
    delete mHits[i];
    mHits[i] = 0;
  }
  mHits.clear();
}

const StSPtrVecMcSsdHit&
StMcSsdWaferHitCollection::hits() const { return mHits; }

StSPtrVecMcSsdHit&
StMcSsdWaferHitCollection::hits() { return mHits; }
