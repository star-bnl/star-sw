/***************************************************************************
 *
 * $Id: StMcRichHitCollection.hh,v 2.1 2000/03/06 18:05:22 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, March 2000
 ***************************************************************************
 *
 * Description: Container for StMcRichHit
 *
 ***************************************************************************
 *
 * $Log: StMcRichHitCollection.hh,v $
 * Revision 2.1  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#ifndef StMcRichHitCollection_hh
#define StMcRichHitCollection_hh
#include "StMcContainers.hh"

class StMcRichHit;

class StMcRichHitCollection {
public:
    StMcRichHitCollection();
    ~StMcRichHitCollection();
    // StMcRichHitCollection(const StMcRichHitCollection&);            use default
    // StMcRichHitCollection& operator=(const StMcRichHitCollection&); use default
    
    bool          addHit(StMcRichHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcRichHit&       hits();
    const StSPtrVecMcRichHit& hits() const;

private:
    StSPtrVecMcRichHit mHits;
};
#endif
