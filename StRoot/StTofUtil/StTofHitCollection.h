/****************************************************************
 * $Id: StTofHitCollection.h,v 1.1 2003/08/08 00:18:26 geurts Exp $
 *****************************************************************
 * Author: Wei-Ming Zhang, April 2001
 * Description: Local TOF hits collection
 *****************************************************************
 * $Log: StTofHitCollection.h,v $
 * Revision 1.1  2003/08/08 00:18:26  geurts
 * moved from StTofMaker to StTofUtil
 *
 *
 ****************************************************************/
#ifndef ST_TOF_HIT_COLLECTION_H
#define ST_TOF_HIT_COLLECTION_H

#include <vector>
#include <cstring>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

class StTofHit;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofHit*> hitVector;
#else
typedef vector<StTofHit*, allocator<StTofHit*> > hitVector;
#endif

class StTofHitCollection {
public:
    StTofHitCollection();
    virtual ~StTofHitCollection();

    bool       push_back(StTofHit* hit);
    size_t     size()  const;
    StTofHit*  front() const;
    StTofHit*  back()  const;
    StTofHit*  getHit(size_t index) const;

    void clear();
    
private:
    hitVector         mHitVector;
};
#endif
