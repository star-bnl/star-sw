/****************************************************************
 * $Id: StRichSingleMCPixel.cxx,v 1.2 2000/05/17 22:29:14 lasiuk Exp $
 *
 * Description:
 *  Definition of a single MC pixel object
 *  
 ****************************************************************
 *
 * $Log: StRichSingleMCPixel.cxx,v $
 * Revision 1.2  2000/05/17 22:29:14  lasiuk
 * keep charge info as a float only.  Access with charge() uniformly
 *
 * Revision 1.2  2000/05/17 22:29:14  lasiuk
 * keep charge info as a float only.  Access with charge() uniformly
 *
 * Revision 1.1  2000/04/05 15:55:24  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#include "StRichSingleMCPixel.h"

StRichSingleMCPixel::StRichSingleMCPixel()
    : StRichSinglePixel(), mMCInfo(0) { /*nopt*/ }

StRichSingleMCPixel::StRichSingleMCPixel(int p, int r, float q)
    : StRichSinglePixel(p, r, q) {/* nopt */}

StRichSingleMCPixel::StRichSingleMCPixel(int p, int r, float q, anIDList info)
    : StRichSinglePixel(p, r, q), mMCInfo(info) {/* nopt */}
    
StRichSingleMCPixel::~StRichSingleMCPixel() { /*nopt */ }

const anIDList& StRichSingleMCPixel::MCInfo() const
{
    return mMCInfo;
}

void StRichSingleMCPixel::setMCInfo(const anIDList& id)
{
    mMCInfo = id;
}
//
// Non-member functions
ostream& operator<<(ostream& os, const StRichSingleMCPixel& pix)
{
    return (os << "StRichSingleMCPixel:=> ("
	    << pix.row()       << ", "
	    << pix.pad()       << ", "
	    << pix.charge()    << ") #=> "
	    << pix.MCInfo().size());
}
