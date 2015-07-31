/****************************************************************  
 * $Id: StBTofRawHitCollection.h,v 1.4 2015/07/28 23:03:05 smirnovd Exp $
 *****************************************************************
 * Author: Xin Dong
 * Description: Local TOF raw hit collection
 *****************************************************************
 * $Log: StBTofRawHitCollection.h,v $
 * Revision 1.4  2015/07/28 23:03:05  smirnovd
 * Removed headers included twice by mistake while merging
 *
 * Revision 1.3  2015/07/28 22:55:43  smirnovd
 * Added cstddef C++ header defining size_t type
 *
 * Revision 1.2  2015/07/28 14:45:55  jeromel
 * Ill-defined size_t ambiguity removed by adding cstddef
 *
 * Revision 1.1  2009/02/02 21:57:21  dongx
 * first release
 *
 *
 ****************************************************************/
#ifndef ST_BTOF_RAWHIT_COLLECTION_H
#define ST_BTOF_RAWHIT_COLLECTION_H

#include <cstddef>
#include <vector>
#include <string>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

//#include "StBTofRawHit.h"
class StBTofRawHit;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StBTofRawHit*> rawHitVector;
#else
typedef vector<StBTofRawHit*, allocator<StBTofRawHit*> > rawHitVector;
#endif

/**
   \class StBTofRawHitCollection
   Class used as a collection for BTofRawHit in StBTofCollection.
 */
class StBTofRawHitCollection {
public:
    /// Default constructor
    StBTofRawHitCollection();
    virtual ~StBTofRawHitCollection();
    
    /// Add a StBTofRawHit into the vector
    bool        push_back(StBTofRawHit* hit);
    /// Returns the size of the collection vector
    size_t      size()  const;
    /// Returns the first element of the vector
    StBTofRawHit*  front() const;
    /// Returns the last element of the vector
    StBTofRawHit*  back()  const;
    /// Returns a BTofRawHit at index in the vector
    StBTofRawHit*  getRawHit(size_t index) const;
    void clear();    
    
private:
    /// StBTofRawHit vector
    rawHitVector         mHitVector;
};
#endif
