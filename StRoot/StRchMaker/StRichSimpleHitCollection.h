/****************************************************************
 * $Id: StRichSimpleHitCollection.h,v 1.1 2000/05/18 12:24:12 lasiuk Exp $
 *
 * Description:
 *  Container for output of cluster finder
 * used to be: : StRichSingleHitCollection.h
 *
 *  Usage:
 *
 ****************************************************************
 *
 * $Log: StRichSimpleHitCollection.h,v $
 * Revision 1.1  2000/05/18 12:24:12  lasiuk
 * nitial revision
 *
 ****************************************************************/
#ifndef ST_RICH_SIMPLEHIT_COLLECTION_H
#define ST_RICH_SIMPLEHIT_COLLECTION_H

#ifdef __ROOT__
#include "TObject.h"
#endif

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StRichSimpleHit.h"

class StRichSimpleHitCollection
#ifdef __ROOT__
    : public TObject {
#endif
public:
    StRichSimpleHitCollection();
    StRichSimpleHitCollection(HitVector&);
    
    virtual ~StRichSimpleHitCollection();

    // ?? Do not use default
    //StRichSimpleHitCollection(const StRichSimpleHitCollection& old);
    //StRichSimpleHitCollection& operator=(const StRichSimpleHitCollection& old);

public:
    HitVector mTheHits;//!
};

#endif /* _H */
