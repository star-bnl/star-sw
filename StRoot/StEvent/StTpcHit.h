/*!
 * \class StTpcHit
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StTpcHit.h,v 2.12 2007/10/03 21:47:36 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.h,v $
 * Revision 2.12  2007/10/03 21:47:36  ullrich
 * Added several new member to hold hit length info.
 *
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
	   unsigned short idTruth=0, unsigned short quality=0,
	   unsigned short id =0,
	   short mnpad=0, short mxpad=0, short mntmbk=0,
	   short mxtmbk=0, float cl_x = 0, float = 0);
    StTpcHit(const dst_point_st&);
    // StTpcHit(const StTpcHit&);            use default
    // StTpcHit& operator=(const StTpcHit&); use default
    ~StTpcHit();

    void* operator new(size_t sz,void *p) { return p;}
    void* operator new(size_t) { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    void           setChargeModified(float charge);
    unsigned int   sector() const;
    unsigned int   padrow() const;
    unsigned int   padsInHit() const;
    unsigned int   pixelsInHit() const;
    unsigned char  minPad() const;
    unsigned char  maxPad() const;
    short          minTmbk() const;
    short          maxTmbk() const;
    virtual int    volumeID() const;
    short          timeBucketsInHit() const;
    float          timeBucket() const;
    float          pad() const;
    float          chargeModified() const;

protected:
    static StMemoryPool mPool;  //!
    UChar_t     mMinpad;     /* central pad - lowest pad id in this hit*/
    UChar_t     mMaxpad;     /* highest pad id in this hit - central pad */
    UChar_t     mMintmbk;    /* central timebucket - lowest time bucket in hit*/
    UChar_t     mMaxtmbk;    /* highest time bucket in hit - central timebucket */
    Short_t     mMcl_x;      /* average pad*64 */
    Short_t     mMcl_t;      /* average timebucket*64 */
    Float_t     mChargeModified; //!
    ClassDef(StTpcHit,4)
};

//
//  Inline functions (having them here makes the interface above easier to read)
//
inline void           StTpcHit::setChargeModified(float charge) {mChargeModified = charge;}
inline unsigned int   StTpcHit::sector() const {return bits(4, 5);}   // bits 4-8  -> 1-24
inline unsigned int   StTpcHit::padrow() const {return bits(9, 6);}   // bits 9-14 -> 1-45
inline unsigned int   StTpcHit::padsInHit()   const {return bits(15, 7);}    // bits 15-21
inline unsigned int   StTpcHit::pixelsInHit() const {return bits(22,10);};   // bits 22-31 obsolete (TCL only, FCF put no. of time buckets)
inline unsigned char  StTpcHit::minPad()   const {return mMcl_x/64 - mMinpad;}
inline unsigned char  StTpcHit::maxPad()   const {return mMcl_x/64 + mMaxpad;}
inline short          StTpcHit::minTmbk()  const {return mMcl_t/64 - mMintmbk;}
inline short          StTpcHit::maxTmbk()  const {return mMcl_t/64 + mMaxtmbk;}
inline int            StTpcHit::volumeID() const {return 100 * sector() + padrow();}
inline short          StTpcHit::timeBucketsInHit()   const {return bits(22,7);} /* number of time bucket fired in this hit */
inline float          StTpcHit::timeBucket() const {return static_cast<float>(mMcl_t)/64.;}
inline float          StTpcHit::pad() const {return static_cast<float>(mMcl_x)/64.;}
inline float          StTpcHit::chargeModified() const {return mChargeModified;}

#endif
