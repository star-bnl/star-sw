/****************************************************************
 * $Id: StRichSingleHitCollection.h,v 1.1 2000/04/05 16:39:51 lasiuk Exp $
 *
 * Description:
 *  Container for output of cluster finder
 *
 *  Usage:
 *
 ****************************************************************
 *
 * $Log: StRichSingleHitCollection.h,v $
 * Revision 1.1  2000/04/05 16:39:51  lasiuk
 * Initial Revision
 *
 * Revision 1.2  2000/05/18 11:42:42  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.1  2000/04/05 16:39:51  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifndef ST_RICH_SINGLEHIT_COLLECTION_H
#define ST_RICH_SINGLEHIT_COLLECTION_H

#ifdef __ROOT__
#include "TObject.h"
#endif

#include <vector>
#ifndef ST_NO_NAMESPACES
#include "StRichHit.h"
#endif

#include "StRichSimpleHit.h"

class StRichSingleHitCollection
#ifdef __ROOT__
    : public TObject {
#endif
public:
    StRichSingleHitCollection();
    StRichSingleHitCollection(HitVector&);
    
    virtual ~StRichSingleHitCollection();

    // ?? Do not use default
    //StRichSingleHitCollection(const StRichSingleHitCollection& old);
    //StRichSingleHitCollection& operator=(const StRichSingleHitCollection& old);

public:
    HitVector mTheHits;//!
};

#endif /* _H */
