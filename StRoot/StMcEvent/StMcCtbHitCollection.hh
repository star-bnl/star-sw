/***************************************************************************
 *
 * $Id: StMcCtbHitCollection.hh,v 2.1 2003/02/19 03:29:42 calderon Exp $
 * $Log: StMcCtbHitCollection.hh,v $
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

class StMcCtbHit;

class StMcCtbHitCollection {
public:
    StMcCtbHitCollection();
    ~StMcCtbHitCollection();
    // StMcCtbHitCollection(const StMcCtbHitCollection&);            use default
    // StMcCtbHitCollection& operator=(const StMcCtbHitCollection&); use default
    
    bool          addHit(StMcCtbHit*);
    unsigned long numberOfHits() const;

    StSPtrVecMcCtbHit&       hits();
    const StSPtrVecMcCtbHit& hits() const;

private:
    StSPtrVecMcCtbHit mHits;
};
#endif
