/***************************************************************************
 *
 * $Id: StMcSvtLadderHitCollection.hh,v 2.1 1999/11/19 19:06:33 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Ladder Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtLadderHitCollection.hh,v $
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:01:00  calderon
 * Completely revised for new StEvent
 *
 *
 **************************************************************************/
#ifndef StMcSvtLadderHitCollection_hh
#define StMcSvtLadderHitCollection_hh

#include "StMcSvtWaferHitCollection.hh"

class StMcSvtLadderHitCollection
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StMcSvtLadderHitCollection();
    ~StMcSvtLadderHitCollection();
    // StMcSvtLadderHitCollection(const StMcSvtLadderHitCollection&); use default
    // const StMcSvtLadderHitCollection& operator=(const StMcSvtLadderHitCollection&);                use default
    
    unsigned long numberOfHits() const;
    unsigned int  numberOfWafers() const;
    
    StMcSvtWaferHitCollection*       wafer(unsigned int);
    const StMcSvtWaferHitCollection* wafer(unsigned int) const;

    void setLayerNumber(int);
    
private:
    enum { mMaxNumberOfWafers = 7 };
    int                        mLayerNumber;
    StMcSvtWaferHitCollection  mWafers[mMaxNumberOfWafers];
    
#ifdef PERSISTENT
    ClassDef(StMcSvtLadderHitCollection,1)
#endif
};
#endif
