/****************************************************************
 * $Id: StRichSinglePixelCollection.h,v 1.1 2000/04/05 16:39:58 lasiuk Exp $
 *
 * Description:
 *  Container for cluster finder which allows access in
 *  a 1d or 2d format.
 *  Based on CERES CCollection, but uses a vector instead
 *  of RWPtrOrderedVector.  Uses STL copy and fill, instead
 *  of memset and memcpy
 *
 *  Usage:
 *  StRichSinglePixelCollection thePixels;
 *  int numberOfRows = 96;
 *  int numberOfColumns = 160;
 *  thePixels.resize(numberOfColumns, numberOfRows);
 *  int pad,row,adc;
 *  thePixels.push_back(new StRichSinglePixel(pad,row,adc));
 *
 ****************************************************************
 *
 * $Log: StRichSinglePixelCollection.h,v $
 * Revision 1.1  2000/04/05 16:39:58  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 16:39:58  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#ifndef ST_RICH_SINGLEPIXEL_COLLECTION_H
#define ST_RICH_SINGLEPIXEL_COLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
using std::uninitialized_fill_n;
#endif


#include "StRrsMaker/StRichRrsMacros.h"
#include "StRrsMaker/StRichSinglePixel.h"

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StRichSinglePixel*> PixelVector;
#else
    vector<StRichSinglePixel*, allocator<StRichSinglePixel*> > PixelVector;
#endif

class StRichSinglePixelCollection {
public:
    StRichSinglePixelCollection();
    virtual ~StRichSinglePixelCollection();

    // Do not use default
    StRichSinglePixelCollection(const StRichSinglePixelCollection& old);
    StRichSinglePixelCollection& operator=(const StRichSinglePixelCollection& old);

    bool                push_back(StRichSinglePixel* pix);
    size_t              size()  const;
    StRichSinglePixel*  front() const;
    StRichSinglePixel*  back()  const;

    // with bounds check
    StRichSinglePixel*& operator[](size_t i);
    StRichSinglePixel* operator[](size_t i) const; // NOT an l-value

    // no bounds check
    StRichSinglePixel*& operator()(size_t i);
    StRichSinglePixel* operator()(size_t i) const; // NOT an l-value

    void resize(size_t x, size_t y);
    void resize(size_t i);
    
    void clear();
    void clearAndDestroy();
    
    bool boundCheck(size_t x, size_t y) const;
    size_t where(size_t x, size_t y) const;
    void clearThePixelArray();
    
protected:
    size_t mMinX;
    size_t mMaxX;
    size_t mMinY;
    size_t mMaxY;

    StRichSinglePixel* *mPixelArray;
    PixelVector         mPixelVector;
    StRichSinglePixel*  mZero;
};

#endif /* _H */
