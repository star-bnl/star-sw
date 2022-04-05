/*!
 * \class StSvtHit 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StSvtHit.h,v 2.18 2016/02/26 14:45:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.h,v $
 * Revision 2.18  2016/02/26 14:45:15  ullrich
 * Implemented detector() which is abstract method inherited from StHit.
 *
 * Revision 2.17  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.16  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.15  2009/11/10 00:41:11  ullrich
 * Changed print-out format and added new method shell().
 *
 * Revision 2.14  2007/09/20 20:02:47  ullrich
 * Added new members to hold and access the number of anodes and of pixels.
 *
 * Revision 2.13  2006/04/27 21:59:00  ullrich
 * Added data member and methods to deal with local positions.
 *
 * Revision 2.12  2005/07/19 21:38:56  perev
 * Cleanup
 *
 * Revision 2.11  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.10  2003/01/08 19:43:11  perev
 * CleanUp
 *
 * Revision 2.9  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.8  2001/08/07 20:50:53  caines
 * Implement better packing of hardware and charge values
 *
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

class StSvtHit : public StHit {
public:
    StSvtHit();
    StSvtHit(const StThreeVectorF&,
             const StThreeVectorF&,
             unsigned int, float, unsigned char = 0);
    // StSvtHit(const StSvtHit&);            use default
    // StSvtHit& operator=(const StSvtHit&); use default
    ~StSvtHit();

    void* operator new(size_t sz,void *p)     { return p;}
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned int layer() const;      // layer=[1,6]
    static unsigned int layer(unsigned int barrel, unsigned int ladder);
    unsigned int ladder() const;     // ladder=[1-8]
    unsigned int wafer() const;      // wafer=[1-7]
    unsigned int barrel() const;     // barrel=[1-3]
    unsigned int hybrid() const;     // hybrid=[1-2]
    unsigned int index() const;  // hybrid index
    float anode() const;  // anode of hit in 1/4 slices
    float timebucket() const; // timebucket of hit in 1/4 slices
    float peakADC() const; // Peak ADC value of hit
    float localPosition(unsigned int) const;
    int numberOfAnodes() const;
    int numberOfPixels() const;

    static unsigned int shell(unsigned int barrel, unsigned int ladder);
    unsigned int shell() const;    
    
    void setPeak(float);
    void setAnode(float);
    void setTimebucket(float);
    void setLocalPosition(float, float);
    virtual int volumeID() const;
    void setNumberOfAnodes(unsigned short);
    void setNumberOfPixels(unsigned short);
    void Print(Option_t *option="") const;
    
    StDetectorId detector() const;

protected:
    static StMemoryPool mPool;  //!
    Float_t mPeak;
    Float_t mAnode;
    Float_t mTimebucket;
    Float_t mLocalPosition[2];
    UShort_t mNumberOfAnodes;
    UShort_t mNumberOfPixels;
    
private:
    enum {mNBarrel=3};
    ClassDef(StSvtHit,3)
};

inline unsigned int
StSvtHit::index() const
{
    // bits 4-13: Hybrid index
    return (mHardwarePosition>>4)&((1L<<9)-1);
}

inline StDetectorId StSvtHit::detector() const {return kSvtId;}
inline float StSvtHit::timebucket() const { return mTimebucket; }
inline void StSvtHit::setPeak(float val) { mPeak = val; }
inline void StSvtHit::setAnode(float val) { mAnode = val; }
inline void StSvtHit::setTimebucket(float val) { mTimebucket = val; }
ostream&              operator<<(ostream& os, StSvtHit const & v);

#endif
