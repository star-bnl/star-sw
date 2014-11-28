/***************************************************************************
 *
 * $Id: StMcCtbHitCollection.hh,v 2.4 2012/03/22 00:33:29 perev Exp $
 * $Log: StMcCtbHitCollection.hh,v $
 * Revision 2.4  2012/03/22 00:33:29  perev
 * private => protected
 *
 * Revision 2.3  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/02/19 03:29:42  calderon
 * Introduction of CTB classes to repository.
 *
 * Revision 1.0  2003/03/18 00:00:00  gans
 * Introduction of Ctb classes.  Modified several classes
 * accordingly.
 */
#ifndef StMcCtbHitCollection_hh
#define StMcCtbHitCollection_hh
#include "StMcContainers.hh"
#include "StObject.h"

class StMcCtbHit;

class StMcCtbHitCollection : public StObject {
public:
    StMcCtbHitCollection();
    virtual ~StMcCtbHitCollection();
    // StMcCtbHitCollection(const StMcCtbHitCollection&);            use default
    // StMcCtbHitCollection& operator=(const StMcCtbHitCollection&); use default
    void Clear(const char* opt="");
    bool IsFolder() const { return true;};
virtual void Browse(TBrowser *b); 
    
    bool          addHit(StMcCtbHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcCtbHit&       hits();
    const StSPtrVecMcCtbHit& hits() const;

protected:
    StSPtrVecMcCtbHit mHits;
    ClassDef(StMcCtbHitCollection,1)
};
#endif
