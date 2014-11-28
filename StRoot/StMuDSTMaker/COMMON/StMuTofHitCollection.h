/****************************************************************
 * $Id: StMuTofHitCollection.h,v 1.2 2004/05/02 04:10:14 perev Exp $
 *
 * Author: Xin Dong, April 2004
 *
 *****************************************************************
 * Description:
 * TOF hits collection for Micro Dst
 *
 *****************************************************************
 *
 * $Log: StMuTofHitCollection.h,v $
 * Revision 1.2  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.1  2004/04/02 03:39:12  jeromel
 * New files for TTOF
 *
 ****************************************************************/

#ifndef ST_MU_TOF_HIT_COLLECTION_H
#define ST_MU_TOF_HIT_COLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif


#include "StMuTofHit.h"

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StMuTofHit*> hitVector;
#else
typedef vector<StMuTofHit*, allocator<StMuTofHit*> > hitVector;
#endif

class StMuTofHitCollection {
public:
    StMuTofHitCollection();
    virtual ~StMuTofHitCollection();

    // Do not use default
//    StMuTofHitCollection(const StMuTofHitCollection& old);
//    StMuTofHitCollection& operator=(const StMuTofHitCollection& old);

    bool       push_back(StMuTofHit* hit);
    size_t     size()  const;
    StMuTofHit*  front() const;
    StMuTofHit*  back()  const;
    StMuTofHit*  getHit(size_t index) const;

    void clear();
    
protected:

    hitVector         mHitVector;
};

#endif /* _TOF */
