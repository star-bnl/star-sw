/***************************************************************************
 *
 * $Id: StRichPixelCollection.cxx,v 2.2 2000/01/31 12:01:03 ullrich Exp $
 *
 * Author: bl, Jan 2000
 ***************************************************************************
 *
 * Description: Keeps track of the coded pixel (raw) data in a TArrayL
 *
 ***************************************************************************
 *
 * $Log: StRichPixelCollection.cxx,v $
 * Revision 2.2  2000/01/31 12:01:03  ullrich
 * Unique const_cast syntax for all platforms.
 *
 * Revision 2.3  2000/02/23 11:45:05  ullrich
 * Added missing ClassImp macro and missing rcsid string.
 *
 * Revision 2.2  2000/01/31 12:01:03  ullrich
 * Unique const_cast syntax for all platforms.
 *
 * Revision 2.1  2000/01/13 21:06:22  lasiuk
 * add rich pixel info/containers
 *
 **************************************************************************/

static const char rcsid[] = "$Id: StRichPixelCollection.cxx,v 2.2 2000/01/31 12:01:03 ullrich Exp $";
  
StRichPixelCollection::StRichPixelCollection()
{
    mPackedData.Set(0);
}

StRichPixelCollection::StRichPixelCollection(dst_rch_pixel_st *theTable, int numberOfPixels)
{
    //
    // check the pointer to see if it exists
    //
    if(theTable) {
	//
	// the length of table (number of pixels)
	//
	mPackedData.Set(numberOfPixels);
	
	for(int ii=0; ii<numberOfPixels; ii++) {
	    mPackedData[ii] = theTable[ii].codedData;
	}
	
    }
}

StRichPixelCollection::~StRichPixelCollection()
{ /* nopt */ }

StRichPixel StRichPixelCollection::pixel(unsigned int index) const
{
    StRichPixel aPixel;

    //
    // check the size; if it is okay, return the item
    //
    if(index < size())
	aPixel.setPackedData((const_cast<TArrayL&>(mPackedData))[index]);
    return aPixel;
}
