/***************************************************************************
 *
 * $Id $
 *
 * Author: Aleksei Pavlinov, May 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Emc Module Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcEmcModuleHitCollection.cc,v $
 * Revision 2.1  2000/06/06 23:01:09  calderon
 * Inital revision
 *
 *
 **************************************************************************/
#include "StMcEmcModuleHitCollection.hh"
#include "StMcCalorimeterHit.hh"

static const char rcsid[] = "$Id: StMcEmcModuleHitCollection.cc,v 2.1 2000/06/06 23:01:09 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StMcEmcModuleHitCollection)
#endif

StMcEmcModuleHitCollection::StMcEmcModuleHitCollection()  { /* noop */ }

StMcEmcModuleHitCollection::~StMcEmcModuleHitCollection()
{
    for (unsigned int i=0; i<mHits.size(); i++) {
        delete mHits[i];
        mHits[i] = 0;
    }
    mHits.clear();
}

unsigned long 
StMcEmcModuleHitCollection::numberOfHits() const
{
    return mHits.size();
}

float 
StMcEmcModuleHitCollection::sum() const
{
    float s = 0.0;
    for(unsigned int i=0; i<mHits.size(); i++){
	s += (*mHits[i]).dE();
    }
    return s;
} 

const StSPtrVecMcCalorimeterHit&
StMcEmcModuleHitCollection::hits() const { return mHits; }

StSPtrVecMcCalorimeterHit&
StMcEmcModuleHitCollection::hits() { return mHits; }
