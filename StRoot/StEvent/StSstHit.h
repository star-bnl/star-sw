/*!
 * \class StSstHit 
 * \author Jonathan Bouchet, Thomas Ullrich, May 2015
 */
/***************************************************************************
 *
 * $Id: StSstHit.h,v 2.3 2016/02/25 17:10:20 ullrich Exp $
 *
 * Author: Jonathan Bouchet, Thomas Ullrich, May 2015
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSstHit.h,v $
 * Revision 2.3  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
 * Revision 2.2  2015/05/21 14:11:43  ullrich
 * Changed mADC from int to unsigned short.
 *
 * Revision 2.1  2015/05/13 16:50:59  ullrich
 * Initial Revision.
 *
  *
 **************************************************************************/
#ifndef StSstHit_hh
#define StSstHit_hh

#include "StHit.h"
#include "StMemoryPool.hh"

class StSstHit : public StHit {
public:
    StSstHit();
    StSstHit(const StThreeVectorF&,
             const StThreeVectorF&,
             unsigned int, float, unsigned char = 0);
    // StSstHit(const StSstHit&);            use default
    // StSstHit& operator=(const StSstHit&); use default
    ~StSstHit();

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
    void         setLocalPosition(float, float, float);
    void         setADC(unsigned short, unsigned short); 
    int          getADC(unsigned int) const; 
    virtual int  volumeID() const;
    void         Print(const Option_t *option="") const;
    
    StDetectorId detector() const;

protected:
    static StMemoryPool mPool;  //!
    Float_t mLocalPosition[3];
    UShort_t   mADC[2]; 
    
private:
    enum {mWaferPerLadder=16};
    
    ClassDef(StSstHit,2)
};

inline StDetectorId StSstHit::detector() const {return kSstId;}

inline unsigned int
StSstHit::sector(unsigned int ladder) {
    if (ladder <=  2 || ladder == 20) return 1;
    if (ladder >=  3 && ladder <=  9) return 2;
    if (ladder >= 10 && ladder <= 12) return 3;
    if (ladder >= 13 && ladder <= 19) return 4;
    return 0;
}

inline unsigned int
StSstHit::sector() const {return sector(ladder()); }

ostream& operator<<(ostream& os, StSstHit const & v);

#endif
