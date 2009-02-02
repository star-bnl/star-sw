/****************************************************************  
 * $Id: StBTofRawHitCollection.h,v 1.1 2009/02/02 21:57:21 dongx Exp $
 *****************************************************************
 * Author: Xin Dong
 * Description: Local TOF raw hit collection
 *****************************************************************
 * $Log: StBTofRawHitCollection.h,v $
 * Revision 1.1  2009/02/02 21:57:21  dongx
 * first release
 *
 *
 ****************************************************************/
#ifndef ST_BTOF_RAWHIT_COLLECTION_H
#define ST_BTOF_RAWHIT_COLLECTION_H

#include <vector>
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
