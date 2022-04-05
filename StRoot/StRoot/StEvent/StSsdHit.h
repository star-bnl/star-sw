/*!
 * \class StSsdHit 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StSsdHit.h,v 2.13 2016/02/25 17:10:20 ullrich Exp $
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
 * Revision 2.13  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
 * Revision 2.12  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.11  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.10  2006/04/27 21:58:53  ullrich
 * Added data member and methods to deal with local positions.
 *
 * Revision 2.9  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2003/01/08 19:43:11  perev
 * CleanUp
 *
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

class StSsdHit : public StHit {
public:
    StSsdHit();
    StSsdHit(const StThreeVectorF&,
             const StThreeVectorF&,
             unsigned int, float, unsigned char = 0);
    // StSsdHit(const StSsdHit&);            use default
    // StSsdHit& operator=(const StSsdHit&); use default
    ~StSsdHit();

    void* operator new(size_t sz,void *p)     { return p;}
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }    

    unsigned int ladder() const;              // ladder=[1-20]
    unsigned int wafer() const;               // wafer=[1-16]
    unsigned int centralStripNSide() const;  
    unsigned int centralStripPSide() const;  
    unsigned int clusterSizeNSide() const;   
    unsigned int clusterSizePSide() const;
    float        localPosition(unsigned int) const;
    static unsigned int sector(unsigned int);
    unsigned int sector() const;
    void         setLocalPosition(float, float);
    virtual int  volumeID() const;
    
    StDetectorId detector() const;
    void         Print(const Option_t *option="") const;
    
    

protected:
    static StMemoryPool mPool;  //!
    Float_t mLocalPosition[2];
    
private:
    enum {mWaferPerLadder=16};
    ClassDef(StSsdHit,2)
};

inline unsigned int
StSsdHit::sector(unsigned int ladder) {
    if (ladder <=  2 || ladder == 20) return 1;
    if (ladder >=  3 && ladder <=  9) return 2;
    if (ladder >= 10 && ladder <= 12) return 3;
    if (ladder >= 13 && ladder <= 19) return 4;
    return 0;
}

inline unsigned int StSsdHit::sector() const {return sector(ladder()); }
inline StDetectorId StSsdHit::detector() const {return static_cast<StDetectorId>(StHit::bits(0, 4));}

ostream&              operator<<(ostream& os, StSsdHit const & v);

#endif
