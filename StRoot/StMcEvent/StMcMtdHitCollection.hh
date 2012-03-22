/***************************************************************************
 *
 * $Id: StMcMtdHitCollection.hh,v 2.3 2012/03/22 00:45:30 perev Exp $
 * $Log: StMcMtdHitCollection.hh,v $
 * Revision 2.3  2012/03/22 00:45:30  perev
 * private => protected
 *
 * Revision 2.2  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.1  2011/10/11 16:22:39  perev
 * Add Mtd
 *
 *
 */
#ifndef StMcMtdHitCollection_hh
#define StMcMtdHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class TBrowser;
class StMcMtdHit;

class StMcMtdHitCollection : public StObject {
public:
    StMcMtdHitCollection();
    virtual ~StMcMtdHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    bool addHit(StMcMtdHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcMtdHit&       hits();
    const StSPtrVecMcMtdHit& hits() const;

protected:
    StSPtrVecMcMtdHit mHits;
    ClassDef(StMcMtdHitCollection,1)
};
#endif
