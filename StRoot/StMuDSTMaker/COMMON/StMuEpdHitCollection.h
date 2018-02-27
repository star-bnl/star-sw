/****************************************************************
 *
 * Author: Mike Lisa
 *
 *****************************************************************
 *
 * Description:
 *  patterned after StMuBTofHitCollection
 *
 *****************************************************************
 *
 * 
 ****************************************************************/

#ifndef ST_MU_EPD_HIT_COLLECTION_H
#define ST_MU_EPD_HIT_COLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif


#include "StMuEpdHit.h"

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StMuEpdHit*> ehitVector;
#else
typedef vector<StMuEpdHit*, allocator<StMuEpdHit*> > ehitVector;
#endif

class StMuEpdHitCollection {
public:
    StMuEpdHitCollection();
    virtual ~StMuEpdHitCollection();

    // Do not use default
//    StMuEpdHitCollection(const StMuEpdHitCollection& old);
//    StMuEpdHitCollection& operator=(const StMuEpdHitCollection& old);

    bool       push_back(StMuEpdHit* hit);
    size_t     size()  const;
    StMuEpdHit*  front() const;
    StMuEpdHit*  back()  const;
    StMuEpdHit*  getHit(size_t index) const;

    void clear();
    
protected:

    ehitVector         mHitVector;
};

#endif
