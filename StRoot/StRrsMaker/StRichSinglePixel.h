/****************************************************************
 * $Id: StRichSinglePixel.h,v 1.2 2000/02/29 18:26:17 lasiuk Exp $
 *
 * Description:
 *  Definition of a single pixel object
 *
 *
 ****************************************************************
 *
 * $Log: StRichSinglePixel.h,v $
 * Revision 1.2  2000/02/29 18:26:17  lasiuk
 * unsigned long in cluster number changed to int
 *
 * Revision 1.3  2000/04/05 16:05:34  lasiuk
 * add operator==
 *
 * Revision 1.2  2000/02/29 18:26:17  lasiuk
 * unsigned long in cluster number changed to int
 *
 * Revision 1.1  2000/02/29 18:14:10  lasiuk
#ifndef ST_RICH_SINGLEPIXEL_H
#define ST_RICH_SINGLEPIXEL_H
 ****************************************************************/
#ifndef ST_RICH_SINGLE_PIXEL_H
using namespace std::stack;
#endif

#include "StRichEnumeratedTypes.h"

    StRichSinglePixel(int p, int r, int adc);
public:
    ~StRichSinglePixel();
    StRichSinglePixel(int p, int r, float q);

    virtual ~StRichSinglePixel();
    

    bool operator==(const StRichSinglePixel&) const;
    
    double amplitude() const;
    int    pad() const;
    int    row() const;
    float  charge() const;

    int clusterNumber() const;
    void setAmplitude(double amp);
    void setPad(int p);
    void setRow(int r);
    void setCharge(float q);
    void setClusterNumber(int number);

    // FLAG OPERATIONS
    bool isSet(StRichSinglePixelFlag f)      const;
private:
    void unSetBit(StRichSinglePixelFlag f);    

    double        mAmplitude;
    int           mPad;
    int           mRow;
    float         mCharge;
    unsigned long mFlags;
    int mClusterNumber;
inline double StRichSinglePixel::amplitude() const { return mAmplitude; }
inline int StRichSinglePixel::pad() const { return mPad; }
inline int StRichSinglePixel::row() const { return mRow; }
inline float StRichSinglePixel::charge() const { return mCharge; }
inline void StRichSinglePixel::setAmplitude(double amp) { mAmplitude = amp; }
inline void StRichSinglePixel::setPad(int p) { mPad = p; }
    return (mPad==pix.pad() && mRow==pix.row() && mCharge==pix.charge());
}
inline void StRichSinglePixel::unSetBit(StRichSinglePixelFlag b) { mFlags &= ~(1<<b);}

inline void StRichSinglePixel::setBit(StRichSinglePixelFlag b) { mFlags |= b; }
inline void StRichSinglePixel::unSetBit(StRichSinglePixelFlag b) { mFlags &= ~(b);}
inline bool StRichSinglePixel::isSet(StRichSinglePixelFlag b) const { return (mFlags & b); }
typedef stack<StRichSinglePixel*, allocator<StRichSinglePixel*> > PixelStack;
#endif

#endif
