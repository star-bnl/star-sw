/***************************************************************************
 *
 * $Id: StMcSvtLadderHitCollection.hh,v 2.3 2000/04/18 23:46:13 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Ladder Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtLadderHitCollection.hh,v $
 * Revision 2.3  2000/04/18 23:46:13  calderon
 * Fix bug in reurning barrel number
 * Enumerations for the Max barrels, ladders & wafers modified for
 * SSD inclusion in current scheme.
 *
 * Revision 2.2  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
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
#ifndef StMcSvtLadderHitCollection_hh
#define StMcSvtLadderHitCollection_hh

#include "StMcSvtWaferHitCollection.hh"

class StMcSvtLadderHitCollection
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

    void setBarrelNumber(int);
    
private:
    enum { mMaxNumberOfWafers = 16 };
    int                        mBarrelNumber;
    StMcSvtWaferHitCollection  mWafers[mMaxNumberOfWafers];
    
};
#endif
