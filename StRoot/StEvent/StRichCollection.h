/***************************************************************************
 *
 * $Id: StRichCollection.h,v 2.2 2001/02/22 21:04:17 lasiuk Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *   Persistent data which is written into StEvent
 *   directly from the reco chain
 *
 ***************************************************************************
 *
 * $Log: StRichCollection.h,v $
 * Revision 2.2  2001/02/22 21:04:17  lasiuk
 * keep the tracks that fly through the RICH in
 * the collection
 *
 * Revision 2.1  2000/05/22 21:48:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StRichCollection_hh
#define StRichCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StRichCluster.h"
#include "StRichHit.h"

class StRichCollection : public StObject {
public:
    StRichCollection();
    ~StRichCollection();
//     StRichCollection(const StRichCollection&) { /* nopt */ }
//     StRichCollection& operator=(const StRichCollection&) {/* use default */}

    const StSPtrVecRichPixel&    getRichPixels() const;
    StSPtrVecRichPixel&          getRichPixels();

    const StSPtrVecRichCluster&  getRichClusters() const;
    StSPtrVecRichCluster&        getRichClusters();

    const StSPtrVecRichHit&      getRichHits() const;
    StSPtrVecRichHit&            getRichHits();

    const StPtrVecTrack&      getTracks() const;
    StPtrVecTrack&            getTracks();

    void addPixel(const StRichPixel*);
    void addCluster(const StRichCluster*);
    void addHit(const StRichHit*);
    void addTrack(const StTrack*);

    Bool_t pixelsPresent()   const;
    Bool_t clustersPresent() const;
    Bool_t hitsPresent()     const;
    
private:
    StSPtrVecRichPixel     mRichPixels;
    StSPtrVecRichCluster   mRichClusters;
    StSPtrVecRichHit       mRichHits;

    StPtrVecTrack          mTracks;
    
    ClassDef(StRichCollection, 1)
};
#endif
