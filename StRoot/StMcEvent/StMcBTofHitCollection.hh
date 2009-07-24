/***************************************************************************
 *
 * $Id: StMcBTofHitCollection.hh,v 2.1 2009/07/24 19:08:06 perev Exp $
 * $Log: StMcBTofHitCollection.hh,v $
 * Revision 2.1  2009/07/24 19:08:06  perev
 * Cleanup + Btof added (Geurts)
 *
 *
 */
#ifndef StMcBTofHitCollection_hh
#define StMcBTofHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class StMcBTofHit;

class StMcBTofHitCollection : public StObject {
public:
    StMcBTofHitCollection();
    virtual ~StMcBTofHitCollection();
    
    bool          addHit(StMcBTofHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcBTofHit&       hits();
    const StSPtrVecMcBTofHit& hits() const;

private:
    StSPtrVecMcBTofHit mHits;
    ClassDef(StMcBTofHitCollection,1)
};
#endif
