/*!
 * \class StSsdHit 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StSsdHit.h,v 2.7 2002/02/22 22:56:50 jeromel Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *         Lilian Martin, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSsdHit.h,v $
 * Revision 2.7  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2001/04/05 04:00:42  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:34:58  perev
 * clone() -> clone() const
 *
 * Revision 2.4  2000/01/05 16:05:39  ullrich
 * Updated for actual use in StEvent. Unpacking changed.
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
#define StSsdHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"
class dst_point_st;

class StSsdHit : public StHit {
public:
    StSsdHit();
    StSsdHit(const StThreeVectorF&,
             const StThreeVectorF&,
             unsigned int, float, unsigned char = 0);
    StSsdHit(const dst_point_st&);
    // StSsdHit(const StSsdHit&);            use default
    // StSsdHit& operator=(const StSsdHit&); use default
    ~StSsdHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }    

    unsigned int ladder() const;              // ladder=[1-20]
    unsigned int wafer() const;               // wafer=[1-16]
    unsigned int centralStripNSide() const;  
    unsigned int centralStripPSide() const;  
    unsigned int clusterSizeNSide() const;   
    unsigned int clusterSizePSide() const;

protected:
    static StMemoryPool mPool;  //!
    StObject* clone() const;
    
private:
    enum {mWaferPerLadder=16};
    ClassDef(StSsdHit,1)
};
#endif
