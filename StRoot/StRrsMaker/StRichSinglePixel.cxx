/****************************************************************
 * $Id: StRichSinglePixel.cxx,v 1.1 2000/02/29 18:14:10 lasiuk Exp $
 *
 * Description:
 *  Definition of a single pixel object
 *  
 ****************************************************************
 *
 * $Log: StRichSinglePixel.cxx,v $
 * Revision 1.1  2000/02/29 18:14:10  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/02/29 18:14:10  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#include <iostream.h>
#include "StRichSinglePixel.h"

StRichSinglePixel::StRichSinglePixel()
    : mFlags(0), mClusterNumber(-1) { /*nopt*/ }

StRichSinglePixel::StRichSinglePixel(int p, int r, int amp)
    : mPad(p), mRow(r), mAmplitude(amp), mFlags(0), mClusterNumber(-1) {/* nopt */}
    
StRichSinglePixel::~StRichSinglePixel() { /*nopt */ }

//
// Non-member functions
ostream& operator<<(ostream& os, const StRichSinglePixel& pix)
{
    return (os << "StRichSinglePixel:=> ("
	    << pix.row()       << ", "
	    << pix.pad()       << ", "
	    << pix.amplitude() << ")" );
}
