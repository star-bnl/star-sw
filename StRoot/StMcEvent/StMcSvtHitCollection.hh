/***************************************************************************
 *
 * $Id: StMcSvtHitCollection.hh,v 2.2 2000/03/06 18:05:22 calderon Exp $
 * $Log: StMcSvtHitCollection.hh,v $
 * Revision 2.2  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.2  1999/09/23 21:25:52  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcSvtHitCollection_hh
#define StMcSvtHitCollection_hh

#include "StMcSvtBarrelHitCollection.hh"
class StMcSvtHit;

class StMcSvtHitCollection {
public:
    StMcSvtHitCollection();
    ~StMcSvtHitCollection();
    // StMcSvtHitCollection(const StMcSvtHitCollection&);            use default
    // StMcSvtHitCollection& operator=(const StMcSvtHitCollection&); use default
    
    bool          addHit(StMcSvtHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfBarrels() const;
    
    StMcSvtBarrelHitCollection*       barrel(unsigned int);
    const StMcSvtBarrelHitCollection* barrel(unsigned int) const;

private:
    enum { mNumberOfBarrels = 3 };
    StMcSvtBarrelHitCollection mBarrels[mNumberOfBarrels];
    
};

#endif
