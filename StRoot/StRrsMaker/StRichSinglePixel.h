/****************************************************************
 * $Id: StRichSinglePixel.h,v 1.6 2000/05/25 21:34:53 fisyak Exp $
 *
 * Description:
 *  Definition of a single pixel object
 * *** typdef at end of file
 *
 *
 ****************************************************************
 *
 * $Log: StRichSinglePixel.h,v $
 * Revision 1.6  2000/05/25 21:34:53  fisyak
 * Make rootcint happy
 *
 * Revision 1.5  2000/05/19 15:44:31  lasiuk
 * clone members added
 *
 * Revision 1.4  2000/05/17 22:29:15  lasiuk
 * keep charge info as a float only.  Access with charge() uniformly
 *
 * Revision 1.3  2000/04/05 16:05:34  lasiuk
 * add operator==
 *
 * Revision 1.2  2000/02/29 18:26:17  lasiuk
 * unsigned long in cluster number changed to int
 *
 * Revision 1.1  2000/02/29 18:14:10  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifndef ST_RICH_SINGLE_PIXEL_H
#define ST_RICH_SINGLE_PIXEL_H
#ifndef __CINT__
#include <iostream.h>
#include <stack>

#ifndef ST_NO_NAMESPACES
using namespace std::stack;
#endif
#endif
#include "StRichEnumeratedTypes.h"

class StRichSinglePixel  {
public:
    StRichSinglePixel();
    StRichSinglePixel(int p, int r, float q);

    virtual ~StRichSinglePixel();
    virtual StRichSinglePixel* clone();
    
    //StRichSinglePixel(const StRichSinglePixel&) {/*use default*/}
    //StRichSinglePixel& operator=(const StRichSinglePixel&) {/*use default*/|

    bool operator==(const StRichSinglePixel&) const;
    
    // access functions
    int    pad() const;
    int    row() const;
    float  charge() const;

    int clusterNumber() const;

    void setPad(int p);
    void setRow(int r);
    void setCharge(float q);
    void setClusterNumber(int number);

    // FLAG OPERATIONS
    bool isSet(StRichSinglePixelFlag f)      const;
    void setBit(StRichSinglePixelFlag f);
    void unSetBit(StRichSinglePixelFlag f);    

protected:
    int           mPad;
    int           mRow;
    float         mCharge;
    unsigned long mFlags;
    int mClusterNumber;
};
#ifndef __CINT__
inline int StRichSinglePixel::pad() const { return mPad; }
inline int StRichSinglePixel::row() const { return mRow; }
inline float StRichSinglePixel::charge() const { return mCharge; }
inline int StRichSinglePixel::clusterNumber() const { return mClusterNumber; }
inline void StRichSinglePixel::setPad(int p) { mPad = p; }
inline void StRichSinglePixel::setRow(int r) { mRow = r; } 
inline void StRichSinglePixel::setCharge(float q) { mCharge = q; }
inline void StRichSinglePixel::setClusterNumber(int number) { mClusterNumber = number; }
inline bool StRichSinglePixel::operator==(const StRichSinglePixel& pix) const
{
    return (mPad==pix.pad() && mRow==pix.row() && mCharge==pix.charge());
}


inline void StRichSinglePixel::setBit(StRichSinglePixelFlag b) { mFlags |= b; }
inline void StRichSinglePixel::unSetBit(StRichSinglePixelFlag b) { mFlags &= ~(b);}
inline bool StRichSinglePixel::isSet(StRichSinglePixelFlag b) const { return (mFlags & b); }
inline StRichSinglePixel* StRichSinglePixel::clone() {return new StRichSinglePixel(*this);}

// Non-member
ostream& operator<<(ostream& os, const StRichSinglePixel& pix);

// typedef
#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef stack<StRichSinglePixel*> PixelStack;
#else
typedef stack<StRichSinglePixel*, allocator<StRichSinglePixel*> > PixelStack;
#endif
#endif
#endif
