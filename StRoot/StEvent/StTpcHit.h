/***************************************************************************
 *
 * $Id: StTpcHit.h,v 2.5 1999/12/13 20:16:27 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.h,v $
 * Revision 2.5  1999/12/13 20:16:27  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.4  1999/12/01 15:56:31  ullrich
 * Renamed xxxInCluster() methods to xxxInHit()
 *
 * Revision 2.3  1999/11/11 10:19:55  ullrich
 * Inlined sector() and padrow().
 *
 * Revision 2.2  1999/11/09 19:35:27  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.1  1999/10/28 22:27:10  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:51  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTpcHit_hh
#define StTpcHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"
class dst_point_st;

class StTpcHit : public StHit {
public:
    StTpcHit();
    StTpcHit(const StThreeVectorF&,
             const StThreeVectorF&,
             ULong_t, Float_t, UChar_t = 0);
    StTpcHit(const dst_point_st&);
    // StTpcHit(const StTpcHit&);            use default
    // StTpcHit& operator=(const StTpcHit&); use default
    ~StTpcHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    ULong_t   sector() const;          // 1-24
    ULong_t   padrow() const;          // 1-45
    ULong_t   padsInHit() const;
    ULong_t   pixelsInHit() const;

protected:
    static StMemoryPool mPool;  //!
    StObject* clone();
    ClassDef(StTpcHit,1)  
};

inline ULong_t
StTpcHit::sector() const
{
    return bits(4, 5);   // bits 4-8, sector=[1,24]
}

inline ULong_t
StTpcHit::padrow() const
{
    return bits(9, 6);   // bits 9-14, padrow=[1-45]
}

#endif
