/***************************************************************************
 *
 * $Id: StMcSvtHitCollection.hh,v 2.1 1999/11/19 19:06:33 calderon Exp $
 * $Log: StMcSvtHitCollection.hh,v $
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

#include "StMcSvtLayerHitCollection.hh"
class StMcSvtHit;

class StMcSvtHitCollection {
public:
    StMcSvtHitCollection();
    ~StMcSvtHitCollection();
    // StMcSvtHitCollection(const StMcSvtHitCollection&);            use default
    // StMcSvtHitCollection& operator=(const StMcSvtHitCollection&); use default
    
    bool          addHit(StMcSvtHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcSvtLayerHitCollection*       layer(unsigned int);
    const StMcSvtLayerHitCollection* layer(unsigned int) const;

private:
    enum { mNumberOfLayers = 6 };
    StMcSvtLayerHitCollection mLayers[mNumberOfLayers];
    
};

#endif
