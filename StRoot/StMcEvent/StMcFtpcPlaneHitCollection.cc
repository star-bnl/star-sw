/***************************************************************************
 *
 * $Id: StMcFtpcPlaneHitCollection.cc,v 2.0 1999/11/17 02:00:59 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Ftpc Plane Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcFtpcPlaneHitCollection.cc,v $
 * Revision 2.0  1999/11/17 02:00:59  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#include "StMcFtpcPlaneHitCollection.hh"
#include "StMcFtpcHit.hh"
static const char rcsid[] = "$Id: StMcFtpcPlaneHitCollection.cc,v 2.0 1999/11/17 02:00:59 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcFtpcPlaneHitCollection)
#endif

StMcFtpcPlaneHitCollection::StMcFtpcPlaneHitCollection() { /* noop */ }

StMcFtpcPlaneHitCollection::~StMcFtpcPlaneHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
  }
}

const StSPtrVecMcFtpcHit&
StMcFtpcPlaneHitCollection::hits() const { return mHits; }

StSPtrVecMcFtpcHit&
StMcFtpcPlaneHitCollection::hits() { return mHits; }

unsigned long StMcFtpcPlaneHitCollection::numberOfHits() const
{
  return mHits.size();
}
