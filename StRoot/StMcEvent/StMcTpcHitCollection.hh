/***************************************************************************
 *
 * $Id: StMcTpcHitCollection.hh,v 2.1 1999/11/19 19:06:34 calderon Exp $
 * $Log: StMcTpcHitCollection.hh,v $
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.2  1999/09/23 21:25:53  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTpcHitCollection_hh
#define StMcTpcHitCollection_hh

#include "StMcTpcSectorHitCollection.hh"

class StMcTpcHit;

class StMcTpcHitCollection {
public:
    StMcTpcHitCollection();
    ~StMcTpcHitCollection();
    // StMcTpcHitCollection(const StMcTpcHitCollection&);            use default
    // StMcTpcHitCollection& operator=(const StMcTpcHitCollection&); use default
    
    bool  addHit(StMcTpcHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfSectors() const;
    
    StMcTpcSectorHitCollection*       sector(unsigned int);
    const StMcTpcSectorHitCollection* sector(unsigned int) const;

private:
    enum { mNumberOfSectors = 24 };
    StMcTpcSectorHitCollection mSectors[mNumberOfSectors];
    
};

#endif
