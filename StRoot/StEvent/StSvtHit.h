/***************************************************************************
 *
 * $Id: StSvtHit.h,v 2.7 2001/04/05 04:00:43 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.h,v $
 * Revision 2.7  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.5  1999/12/13 20:16:22  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
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
             unsigned int, float, unsigned char = 0);
    StSvtHit(const dst_point_st&);
    // StSvtHit(const StSvtHit&);            use default
    // StSvtHit& operator=(const StSvtHit&); use default
    ~StSvtHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned int layer() const;      // layer=[1,6]
    unsigned int ladder() const;     // ladder=[1-8]
    unsigned int wafer() const;      // wafer=[1-7]
    unsigned int barrel() const;     // barrel=[1-3]
    unsigned int hybrid() const;

protected:
    static StMemoryPool mPool;  //!
    StObject* clone() const;
    ClassDef(StSvtHit,1)
};

inline unsigned int
StSvtHit::layer() const
{
    // bits 4-31: 1000*layer + 100*wafer + ladder (Helen, Sep 99)
    return (mHardwarePosition>>4)/1000;
}

inline unsigned int
StSvtHit::ladder() const
{
    // bits 4-31: 1000*layer + 100*wafer + ladder (Helen, Sep 99)
    return (mHardwarePosition>>4)%100;
}

inline unsigned int
StSvtHit::wafer() const
{
    // bits 4-31: 1000*layer + 100*wafer + ladder (Helen, Sep 99)
    return ((mHardwarePosition>>4)%1000)/100;
}

#endif
