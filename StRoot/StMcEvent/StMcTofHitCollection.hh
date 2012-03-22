/***************************************************************************
 *
 * $Id: StMcTofHitCollection.hh,v 2.5 2012/03/22 00:50:46 perev Exp $
 * $Log: StMcTofHitCollection.hh,v $
 * Revision 2.5  2012/03/22 00:50:46  perev
 * private => protected
 *
 * Revision 2.4  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.3  2009/07/24 19:08:09  perev
 * Cleanup + Btof added (Geurts)
 *
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
    virtual ~StMcTofHitCollection();
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    bool          addHit(StMcTofHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcTofHit&       hits();
    const StSPtrVecMcTofHit& hits() const;

protected:
    StSPtrVecMcTofHit mHits;
    ClassDef(StMcTofHitCollection,1)
};
#endif
