/*!
 * \class StMcIgtLayerHitCollection
 * \brief Monte Carlo Igt Layer Hit Collection class
 * Used to access the hits of an Igt layer.
 * \author Gerrit van Nieuwenhuizen, Manuel Calderon de la Barca Sanchez
 * \date   July 2005
 *
 ***************************************************************************
 *
 * $Id: StMcIgtLayerHitCollection.cc,v 2.1 2005/07/07 18:20:49 calderon Exp $
 *
 ***************************************************************************
 *
 * $Log: StMcIgtLayerHitCollection.cc,v $
 * Revision 2.1  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 *
 *
 **************************************************************************/
#include "StMcIgtLayerHitCollection.hh"
#include "StMcIgtHit.hh"
static const char rcsid[] = "$Id: StMcIgtLayerHitCollection.cc,v 2.1 2005/07/07 18:20:49 calderon Exp $";

ClassImp(StMcIgtLayerHitCollection)

StMcIgtLayerHitCollection::StMcIgtLayerHitCollection() { /* noop */ }

StMcIgtLayerHitCollection::~StMcIgtLayerHitCollection()
{
    // If the hit provides its own new/delete operator.
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i]=0;
    }
}

const StSPtrVecMcIgtHit&
StMcIgtLayerHitCollection::hits() const 
{ 
    return mHits; 
}

StSPtrVecMcIgtHit&
StMcIgtLayerHitCollection::hits() 
{ 
    return mHits; 
}

unsigned long StMcIgtLayerHitCollection::numberOfHits() const
{
    return mHits.size();
}
