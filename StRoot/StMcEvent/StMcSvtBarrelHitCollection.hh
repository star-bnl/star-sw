/***************************************************************************
 *
 * $Id: StMcSvtBarrelHitCollection.hh,v 2.1 2000/03/06 18:05:22 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, March 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Barrel Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtBarrelHitCollection.hh,v $
 * Revision 2.1  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#ifndef StMcSvtBarrelHitCollection_hh
#define StMcSvtBarrelHitCollection_hh

#include "StMcSvtLadderHitCollection.hh"


class StMcSvtBarrelHitCollection
{    
public:
    StMcSvtBarrelHitCollection();
    ~StMcSvtBarrelHitCollection();
    // StMcSvtBarrelHitCollection(const StMcSvtBarrelHitCollection&); use default
    // const StMcSvtBarrelHitCollection&
    // operator=(const StMcSvtBarrelHitCollection&);               use default
    
    unsigned long numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StMcSvtLadderHitCollection*       ladder(unsigned int);
    const StMcSvtLadderHitCollection* ladder(unsigned int) const;

    void setBarrelNumber(int);
    
private:
    enum { mMaxNumberOfLadders = 16 };
    int                    mBarrelNumber;
    StMcSvtLadderHitCollection mLadders[mMaxNumberOfLadders];
    
};
#endif
