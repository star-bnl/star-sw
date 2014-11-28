/***************************************************************************
 *
 * $Id: StMcBTofHitCollection.hh,v 2.3 2012/03/22 00:27:04 perev Exp $
 * $Log: StMcBTofHitCollection.hh,v $
 * Revision 2.3  2012/03/22 00:27:04  perev
 * private => protected
 *
 * Revision 2.2  2012/03/01 16:48:29  perev
 * method Browse() added
 *
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
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    bool          addHit(StMcBTofHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcBTofHit&       hits();
    const StSPtrVecMcBTofHit& hits() const;

protected:
    StSPtrVecMcBTofHit mHits;
    ClassDef(StMcBTofHitCollection,1)
};
#endif
