/*!
 * \class StTpcHit 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StTpcHit.h,v 2.11 2004/08/06 15:37:09 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.h,v $
 * Revision 2.11  2004/08/06 15:37:09  fisyak
 * Add clster id
 *
 * Revision 2.10  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.9  2003/01/08 19:43:11  perev
 * CleanUp
 *
 * Revision 2.8  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.7  2001/04/05 04:00:44  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
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
             unsigned int, float, unsigned char = 0,
	     UShort_t idTruth=0, UShort_t quality=0, UShort_t id =0);
    StTpcHit(const dst_point_st&);
    // StTpcHit(const StTpcHit&);            use default
    // StTpcHit& operator=(const StTpcHit&); use default
    ~StTpcHit();

    void* operator new(size_t sz,void *p)     { return p;}
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned int   sector() const;          // 1-24
    unsigned int   padrow() const;          // 1-45
    unsigned int   padsInHit() const;
    unsigned int   pixelsInHit() const;

protected:
    static StMemoryPool mPool;  //!
    ClassDef(StTpcHit,1)
};

inline unsigned int
StTpcHit::sector() const
{
    return bits(4, 5);   // bits 4-8, sector=[1,24]
}

inline unsigned int
StTpcHit::padrow() const
{
    return bits(9, 6);   // bits 9-14, padrow=[1-45]
}

#endif
