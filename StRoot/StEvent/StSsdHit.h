/***************************************************************************
 *
 * $Id: StSsdHit.h,v 2.1 1999/10/13 19:43:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHit.h,v $
 * Revision 2.1  1999/10/13 19:43:42  ullrich
 * Initial Revision
 *
 * Revision 2.3  1999/11/09 19:35:17  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/10/28 22:26:39  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSsdHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"
class dst_point_st;

class StSsdHit : public StHit {
public:
    StSsdHit();
    StSsdHit(const StThreeVectorF&,
             const StThreeVectorF&,
             ULong_t, Float_t, UChar_t = 0);
    StSsdHit(const dst_point_st&);

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
    
    ULong_t  clusterSizeNSide() const;
    ULong_t  clusterSizePSide() const;

    ClassDef(StSsdHit,1)  //StSsdHit structure
    static StMemoryPool mPool;  //!
    StObject* clone();
    ClassDef(StSsdHit,1)
};
#endif
