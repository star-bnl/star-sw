/****************************************************************
 * $Id: StTofHitCollection.h,v 1.3 2001/09/28 18:40:03 llope Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Local TOF hits collection
 *
 *****************************************************************
 *
 * $Log: StTofHitCollection.h,v $
 * Revision 1.3  2001/09/28 18:40:03  llope
 * first release
 *
 * Revision 1.1  2001/04/24 20:27:37  wzhang
 * First release
 *
 *
 ****************************************************************/

#ifndef ST_TOF_HIT_COLLECTION_H
#define ST_TOF_HIT_COLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif


#include "StTofHit.h"

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofHit*> hitVector;
#else
typedef vector<StTofHit*, allocator<StTofHit*> > hitVector;
#endif

class StTofHitCollection {
public:
    StTofHitCollection();
    virtual ~StTofHitCollection();

    // Do not use default
//    StTofHitCollection(const StTofHitCollection& old);
//    StTofHitCollection& operator=(const StTofHitCollection& old);

    bool       push_back(StTofHit* hit);
    size_t     size()  const;
    StTofHit*  front() const;
    StTofHit*  back()  const;
    StTofHit*  getHit(size_t index) const;

    void clear();
    
private:

    hitVector         mHitVector;
};

#endif /* _TOF */
