/***************************************************************************
 *
 * $Id: StSvtHit.h,v 2.1 1999/10/28 22:26:44 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.h,v $
 * Revision 2.1  1999/10/28 22:26:44  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
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

    ULong_t layer() const;
    ULong_t ladder() const;
    ULong_t wafer() const;
    ULong_t barrel() const;
    ULong_t wafer() const;      // wafer=[0-6]
    ClassDef(StSvtHit,1)  //StSvtHit structure
    ULong_t hybrid() const;

protected:
    static StMemoryPool mPool;  //!
    return ((mHardwarePosition>>4)%1000)/100 - 1;
}

#endif
