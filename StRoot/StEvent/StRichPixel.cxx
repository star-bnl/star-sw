/***************************************************************************
 *
 * $Id: StRichPixel.cxx,v 2.5 2001/04/05 04:00:53 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPixel.cxx,v $
 * Revision 2.5  2001/04/05 04:00:53  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2000/01/13 21:06:19  lasiuk
 * add rich pixel info/containers
 *
 * Revision 2.3  2000/01/10 17:12:21  lasiuk
 * remove dst_rch_pixel dependency;
 * change stored data to a single long;
 * modify unpacking routines;
 *
 * Revision 2.2  1999/10/28 22:26:22  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:05  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichPixel.h"

static const char rcsid[] = "$Id: StRichPixel.cxx,v 2.5 2001/04/05 04:00:53 ullrich Exp $";

ClassImp(StRichPixel)

StRichPixel::StRichPixel()
    : mPackedData(0)
{ /* nopt */ }

StRichPixel::StRichPixel(unsigned int rawData)
   : mPackedData(rawData)
{ /* noop */ }

StRichPixel::~StRichPixel() { /* noop */ }
    
int
StRichPixel::operator==(const StRichPixel& p) const
{
    return (p.mPackedData  == mPackedData);
}

int
StRichPixel::operator!=(const StRichPixel& p) const
{
    return !(*this == p);  // use operator==()
}

