/***************************************************************************
 *
 * $Id: StMcTofHitCollection.hh,v 2.2 2005/01/27 23:40:48 calderon Exp $
 * $Log: StMcTofHitCollection.hh,v $
 * Revision 2.2  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 */
#ifndef StMcTofHitCollection_hh
#define StMcTofHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class StMcTofHit;

class StMcTofHitCollection : public StObject {
public:
    StMcTofHitCollection();
    ~StMcTofHitCollection();
    
    bool          addHit(StMcTofHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcTofHit&       hits();
    const StSPtrVecMcTofHit& hits() const;

private:
    StSPtrVecMcTofHit mHits;
    ClassDef(StMcTofHitCollection,1)
};
#endif
