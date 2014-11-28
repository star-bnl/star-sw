/***************************************************************************
 *
 * $Id: StMcTpcHitCollection.hh,v 2.4 2012/03/22 00:50:46 perev Exp $
 * $Log: StMcTpcHitCollection.hh,v $
 * Revision 2.4  2012/03/22 00:50:46  perev
 * private => protected
 *
 * Revision 2.3  2009/07/24 19:08:09  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.2  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
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

class StMcTpcHitCollection : public StObject {
public:
    StMcTpcHitCollection();
    virtual ~StMcTpcHitCollection();
    // StMcTpcHitCollection(const StMcTpcHitCollection&);            use default
    // StMcTpcHitCollection& operator=(const StMcTpcHitCollection&); use default
    
    bool  addHit(StMcTpcHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfSectors() const;
    
    StMcTpcSectorHitCollection*       sector(unsigned int);
    const StMcTpcSectorHitCollection* sector(unsigned int) const;

protected:
    enum { mNumberOfSectors = 24 };
    StMcTpcSectorHitCollection mSectors[mNumberOfSectors];
    ClassDef(StMcTpcHitCollection,1)
};

#endif
