/****************************************************************
 * $Id: StRichSingleMCPixel.cxx,v 1.1 2000/04/05 15:55:24 lasiuk Exp $
 *
 * Description:
 *  Definition of a single MC pixel object
 *  
 ****************************************************************
 *
 * $Log: StRichSingleMCPixel.cxx,v $
 * Revision 1.1  2000/04/05 15:55:24  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 15:55:24  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#include "StRichSingleMCPixel.h"

StRichSingleMCPixel::StRichSingleMCPixel(int p, int r, int amp, anIDList info)
    : StRichSinglePixel(p, r, amp), mMCInfo(info) {/* nopt */}

StRichSingleMCPixel::StRichSingleMCPixel(int p, int r, float q, anIDList info)
    : StRichSinglePixel(p, r, q), mMCInfo(info) {/* nopt */}
    
StRichSingleMCPixel::~StRichSingleMCPixel() { /*nopt */ }

const anIDList& StRichSingleMCPixel::MCInfo() const
{
    mMCInfo = id;
}
//
// Non-member functions
ostream& operator<<(ostream& os, const StRichSingleMCPixel& pix)
{
	    << pix.amplitude() << ") #=> "
	    << pix.row()       << ", "
	    << pix.pad()       << ", "
	    << pix.charge()    << ") #=> "
	    << pix.MCInfo().size());
}
