/***************************************************************************
 *
 * $Id: StRichPixelCollection.h,v 2.1 2000/01/13 21:06:22 lasiuk Exp $
 *
 * Author: bl, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPixelCollection.h,v $
 * Revision 2.1  2000/01/13 21:06:22  lasiuk
 * add rich pixel info/containers
 *
 * Revision 2.1  2000/01/13 21:06:22  lasiuk
 * add rich pixel info/containers
 *
 **************************************************************************/
#ifndef StRichPixelCollection_hh
#define StRichPixelCollection_hh

#include <iostream.h>
#include "StObject.h"
#include "TArrayL.h"

#include "StRichPixel.h"

class dst_rch_pixel_st;

class StRichPixelCollection : public StObject {
public:
    StRichPixelCollection();
    StRichPixelCollection(dst_rch_pixel_st*, int);
    virtual ~StRichPixelCollection();

    //StRichPixelCollection& operator=(const StRichPixelCollection&);
    //StRichPixelCollection(const StRichPixelCollection&);

    StRichPixel  pixel(unsigned int)       const;  // return a pixel
    unsigned int size()                    const;  // STL size of container
    unsigned int numberOfPixels()          const; 

private:
    TArrayL  mPackedData;

    ClassDef(StRichPixelCollection,1)
};

inline unsigned int
StRichPixelCollection::size() const
{
    return mPackedData.GetSize();
}

inline unsigned int
StRichPixelCollection::numberOfPixels() const
{
    return this->size();
}
#endif
