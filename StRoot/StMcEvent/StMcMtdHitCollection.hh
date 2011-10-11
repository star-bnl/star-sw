/***************************************************************************
 *
 * $Id: StMcMtdHitCollection.hh,v 2.1 2011/10/11 16:22:39 perev Exp $
 * $Log: StMcMtdHitCollection.hh,v $
 * Revision 2.1  2011/10/11 16:22:39  perev
 * Add Mtd
 *
 *
 */
#ifndef StMcMtdHitCollection_hh
#define StMcMtdHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class StMcMtdHit;

class StMcMtdHitCollection : public StObject {
public:
    StMcMtdHitCollection();
    virtual ~StMcMtdHitCollection();
    void Clear(const char* opt="");
    bool          addHit(StMcMtdHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcMtdHit&       hits();
    const StSPtrVecMcMtdHit& hits() const;

private:
    StSPtrVecMcMtdHit mHits;
    ClassDef(StMcMtdHitCollection,1)
};
#endif
