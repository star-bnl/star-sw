/*!
 * \class StTpcHit
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StTpcHit.h,v 2.16 2010/08/31 20:01:04 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.h,v $
 * Revision 2.16  2010/08/31 20:01:04  fisyak
 * Fix no. of padsInHit, accounting afterburner
 *
 * Revision 2.15  2010/03/26 13:47:29  fisyak
 * Add methods to modify hit content
 *
 * Revision 2.14  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.13  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
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
#include "TMath.h"
class StTpcHit : public StHit {
public:
  StTpcHit() : StHit() {mMinpad = mMaxpad = mMintmbk = mMaxtmbk = 0; mMcl_x = mMcl_t = 0; mChargeModified = 0;}
    StTpcHit(const StThreeVectorF& p,
             const StThreeVectorF& e,
             UInt_t hw, float q, UChar_t c = 0,
	     UShort_t idTruth=0, UShort_t quality=0,
	     UShort_t id =0,
	     Short_t mnpad=0, Short_t mxpad=0, Short_t mntmbk=0,
	     Short_t mxtmbk=0, Float_t cl_x = 0, Float_t cl_t = 0) 
      :  StHit(p, e, hw, q, c, idTruth, quality, id) {setExtends(cl_x, cl_t, mnpad, mxpad, mntmbk, mxtmbk); mChargeModified = 0;}
    ~StTpcHit() {}

    void* operator new(size_t sz,void *p) { return p;}
    void* operator new(size_t) { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    void     setChargeModified(Float_t charge) {mChargeModified = charge;}
    void     setPadTmbk(Float_t cl_x, Float_t cl_t) { mMcl_x = TMath::Nint(cl_x*64);  mMcl_t = TMath::Nint(cl_t*64);}
    void     setExtends(Float_t cl_x, Float_t cl_t, Short_t mnpad, Short_t mxpad, Short_t mntmbk, Short_t mxtmbk);
    UInt_t   sector() const {return bits(4, 5);}   // bits 4-8  -> 1-24
    UInt_t   padrow() const {return bits(9, 6);}   // bits 9-14 -> 1-45
    UInt_t   padsInHit()   const {return maxPad() - minPad() + 1;} 
    UInt_t   pixelsInHit() const {return bits(22,10);};   // bits 22-31 obsolete (TCL only, FCF put no. of time buckets)
    UChar_t  minPad()   const {return TMath::Nint(mMcl_x/64.) - mMinpad;}
    UChar_t  maxPad()   const {return TMath::Nint(mMcl_x/64.) + mMaxpad;}
    Short_t  minTmbk()  const {return TMath::Nint(mMcl_t/64.) - mMintmbk;}
    Short_t  maxTmbk()  const {return TMath::Nint(mMcl_t/64.) + mMaxtmbk;}
    Int_t    volumeID() const {return 100 * sector() + padrow();}
    Short_t  timeBucketsInHit()   const {return bits(22,7);} // number of time bucket fired in this hit
    Float_t  timeBucket() const {return static_cast<float>(mMcl_t)/64.;}
    Float_t  pad() const {return static_cast<float>(mMcl_x)/64.;}
    Float_t  chargeModified() const {return mChargeModified;}
    void     Print(Option_t *option="") const;
    
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
ostream&              operator<<(ostream& os, StTpcHit const & v);
#endif
