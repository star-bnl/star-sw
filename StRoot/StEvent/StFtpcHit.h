/***************************************************************************
 *
 * $Id: StFtpcHit.h,v 2.4 1999/12/13 20:16:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.h,v $
 * Revision 2.4  1999/12/13 20:16:15  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.3  1999/12/06 18:28:24  ullrich
 * Changed method names xxxInCluster to xxxInHit
 *
 * Revision 2.2  1999/11/09 19:35:12  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.1  1999/10/28 22:25:19  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:07  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StFtpcHit_hh
#define StFtpcHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"
class dst_point_st;

class StFtpcHit : public StHit {
public:
    StFtpcHit();
    StFtpcHit(const StThreeVectorF&,
              const StThreeVectorF&,
              ULong_t, Float_t, UChar_t = 0);
    StFtpcHit(const dst_point_st&);
    // StFtpcHit(const StFtpcHit&);            use default
    // StFtpcHit& operator=(const StFtpcHit&); use default
    ~StFtpcHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
  
    ULong_t sector() const;        // 1-6
    ULong_t plane() const;         // 1-20
    ULong_t padsInHit() const;
    ULong_t timebinsInHit() const;

protected:    
    static StMemoryPool mPool;  //!
    StObject* clone();
    ClassDef(StFtpcHit,1) 
};
#endif
