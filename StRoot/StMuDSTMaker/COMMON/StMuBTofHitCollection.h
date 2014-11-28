/****************************************************************
 * $Id: StMuBTofHitCollection.h,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Feb. 2009
 *
 *****************************************************************
 *
 * Description:
 * TOF hits collection for Micro Dst
 *
 *****************************************************************
 *
 * $Log: StMuBTofHitCollection.h,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 * 
 ****************************************************************/

#ifndef ST_MU_BTOF_HIT_COLLECTION_H
#define ST_MU_BTOF_HIT_COLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif


#include "StMuBTofHit.h"

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StMuBTofHit*> bhitVector;
#else
typedef vector<StMuBTofHit*, allocator<StMuBTofHit*> > bhitVector;
#endif

class StMuBTofHitCollection {
public:
    StMuBTofHitCollection();
    virtual ~StMuBTofHitCollection();

    // Do not use default
//    StMuBTofHitCollection(const StMuBTofHitCollection& old);
//    StMuBTofHitCollection& operator=(const StMuBTofHitCollection& old);

    bool       push_back(StMuBTofHit* hit);
    size_t     size()  const;
    StMuBTofHit*  front() const;
    StMuBTofHit*  back()  const;
    StMuBTofHit*  getHit(size_t index) const;

    void clear();
    
protected:

    bhitVector         mHitVector;
};

#endif /* _TOF */
