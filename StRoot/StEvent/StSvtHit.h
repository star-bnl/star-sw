/***************************************************************************
 *
 * $Id: StSvtHit.h,v 2.4 1999/11/11 11:03:57 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.h,v $
 * Revision 2.4  1999/11/11 11:03:57  ullrich
 * Inlined layer(), sector() and ladder().
 *
 * Revision 2.4  1999/11/11 11:03:57  ullrich
 * Inlined layer(), sector() and ladder().
 *
 * Revision 2.3  1999/11/09 19:35:22  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
 * Revision 2.2  1999/11/04 21:40:57  ullrich
 * Added missing default constructor
 *
 * Revision 2.1  1999/10/28 22:26:44  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:45  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StSvtHit_hh
#define StSvtHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"
class dst_point_st;

class StSvtHit : public StHit {
public:
    StSvtHit();
    StSvtHit(const StThreeVectorF&,
             const StThreeVectorF&,
             ULong_t, Float_t, UChar_t = 0);
    StSvtHit(const dst_point_st&);
    // StSvtHit(const StSvtHit&);            use default
    // StSvtHit& operator=(const StSvtHit&); use default
    ~StSvtHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    ULong_t layer() const;      // layer=[0,5]
    ULong_t ladder() const;     // ladder=[0-7]
    ULong_t wafer() const;      // wafer=[0-6]
    ULong_t barrel() const;     // barrel=[0-2]
    ULong_t hybrid() const;

protected:
    static StMemoryPool mPool;  //!
    StObject* clone();
    ClassDef(StSvtHit,1)
};

inline ULong_t
StSvtHit::layer() const
{
    // bits 4-31: 1000*layer + 100*wafer + ladder (Helen, Sep 99)
    return (mHardwarePosition>>4)/1000 - 1;
}

inline ULong_t
StSvtHit::ladder() const
{
    // bits 4-31: 1000*layer + 100*wafer + ladder (Helen, Sep 99)
    return (mHardwarePosition>>4)%100 - 1;
}

inline ULong_t
StSvtHit::wafer() const
{
    // bits 4-31: 1000*layer + 100*wafer + ladder (Helen, Sep 99)
    return ((mHardwarePosition>>4)%1000)/100 - 1;
}

#endif
