/***************************************************************************
 *
 * $Id: StMcSvtLayerHitCollection.hh,v 2.0 1999/11/17 02:01:00 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Layer Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtLayerHitCollection.hh,v $
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcSvtLayerHitCollection_hh
#define StMcSvtLayerHitCollection_hh

#include "StMcSvtLadderHitCollection.hh"

#ifdef PERSISTENT
#include "StObject.h"
#endif

class StMcSvtLayerHitCollection
#ifdef PERSISTENT
    : public StObject
#endif
{    
public:
    StMcSvtLayerHitCollection();
    ~StMcSvtLayerHitCollection();
    // StMcSvtLayerHitCollection(const StMcSvtLayerHitCollection&); use default
    // const StMcSvtLayerHitCollection&
    // operator=(const StMcSvtLayerHitCollection&);               use default
    
    unsigned long numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StMcSvtLadderHitCollection*       ladder(unsigned int);
    const StMcSvtLadderHitCollection* ladder(unsigned int) const;

    void setLayerNumber(int);
    
private:
    enum { mMaxNumberOfLadders = 8 };
    int                    mLayerNumber;
    StMcSvtLadderHitCollection mLadders[mMaxNumberOfLadders];
    
#ifdef PERSISTENT
    ClassDef(StMcSvtLayerHitCollection,1)
#endif
};
#endif
