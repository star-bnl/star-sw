/****************************************************************
 * $Id: StRichSinglePixel.cxx,v 1.3 2000/05/17 22:29:14 lasiuk Exp $
 *
 * Description:
 *  Definition of a single pixel object
 *  
 ****************************************************************
 *
 * $Log: StRichSinglePixel.cxx,v $
 * Revision 1.3  2000/05/17 22:29:14  lasiuk
 * keep charge info as a float only.  Access with charge() uniformly
 *
 * Revision 1.2  2000/04/05 16:05:30  lasiuk
 * add operator==
 *
 * Revision 1.1  2000/02/29 18:14:10  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#include "StRichSinglePixel.h"

StRichSinglePixel::StRichSinglePixel()
    : mFlags(0), mClusterNumber(-1) { /*nopt*/ }

StRichSinglePixel::StRichSinglePixel(int p, int r, float q)
    : mPad(p), mRow(r), mCharge(q), mFlags(0), mClusterNumber(-1) {/* nopt */}
    
StRichSinglePixel::~StRichSinglePixel() { /*nopt */ }

//
// Non-member functions
ostream& operator<<(ostream& os, const StRichSinglePixel& pix)
{
    return (os << "StRichSinglePixel:=> ("
	    << pix.pad()       << ", "
	    << pix.row()       << ", "
	    << pix.charge() << ")" );
}
