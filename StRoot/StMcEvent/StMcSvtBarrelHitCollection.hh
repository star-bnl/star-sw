/***************************************************************************
 *
 * $Id: StMcSvtBarrelHitCollection.hh,v 2.5 2012/03/22 00:48:45 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, March 2000
 ***************************************************************************
 *
 * Description: Monte Carlo Svt Barrel Hit Collection class
 *
 ***************************************************************************
 *
 * $Log: StMcSvtBarrelHitCollection.hh,v $
 * Revision 2.5  2012/03/22 00:48:45  perev
 * private => protected
 *
 * Revision 2.4  2009/07/24 19:08:08  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.3  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2000/04/18 23:46:12  calderon
 * Fix bug in reurning barrel number
 * Enumerations for the Max barrels, ladders & wafers modified for
 * SSD inclusion in current scheme.
 *
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


class StMcSvtBarrelHitCollection : public StObject
{    
public:
    StMcSvtBarrelHitCollection();
    virtual ~StMcSvtBarrelHitCollection();
    // StMcSvtBarrelHitCollection(const StMcSvtBarrelHitCollection&); use default
    // const StMcSvtBarrelHitCollection&
    // operator=(const StMcSvtBarrelHitCollection&);               use default
    
    unsigned long numberOfHits() const;
    unsigned int  numberOfLadders() const;
    
    StMcSvtLadderHitCollection*       ladder(unsigned int);
    const StMcSvtLadderHitCollection* ladder(unsigned int) const;

    void setBarrelNumber(int);
    
protected:
    enum { mMaxNumberOfLadders = 20 };
    int                    mBarrelNumber;
    StMcSvtLadderHitCollection mLadders[mMaxNumberOfLadders];
    ClassDef(StMcSvtBarrelHitCollection,1)
};
#endif
