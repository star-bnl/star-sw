/******************************************************************
 *  $Id: StRichPadPlane.cxx,v 1.2 2000/04/05 16:03:00 lasiuk Exp $
 *
 * Description:
 *   StRichPadPlane is a special container specialized to store 
 *   RICH Raw Data. 
 ********************************************************************
 *  $Log: StRichPadPlane.cxx,v $
 *  Revision 1.2  2000/04/05 16:03:00  lasiuk
 *  remove size_type type definition.  Clash on SUN
 *
 *  Revision 1.1  2000/01/18 21:32:03  lasiuk
 *  Initial Revision
 *
 ********************************************************************/

#include "StRichPadPlane.h"

ostream& operator<<(ostream& os, const StRichID& id)
{
    return (os << "id.mHitID: " << id.mHitID << " GID: " << id.mG_ID << " track: " << id.mTrackp
	       << " q: " << id.mAmount << " type: " << static_cast<int>(id.mSignalType));
}

StRichPadPlane::StRichPadPlane()
    : v() { /*nopt*/ }
    
StRichPadPlane::StRichPadPlane(size_t rows, size_t cols)
{
    v.resize(rows);
    for (unsigned int i=0; i<rows; i++ )
	v[i].resize(cols);
}

StRichPadPlane::~StRichPadPlane() { /* nopt */ }
    
// row_type&
// StRichPadPlane::operator[](int n)
// { return v[n]; }

// const_row_iter StRichPadPlane::begin() const
// { return v.begin(); }

// row_iter StRichPadPlane::begin()
// { return v.begin(); }

// const_row_iter StRichPadPlane::end() const
// { return v.end(); }

// row_iter StRichPadPlane::end()
// { return v.end(); }

// size_t StRichPadPlane::row_size()
// { return v.size(); }

// size_t StRichPadPlane::col_size()
// { return v.begin()->size(); }

void StRichPadPlane::resize(size_t rows, size_t cols)
{ 
    if ( v.size() != rows )
	v.resize( rows );
    for ( unsigned int i = 0; i < v.size(); i++ )
	v[i].resize(cols);
}

void StRichPadPlane::clear()
{
    unsigned int rows = row_size();
    unsigned int cols = col_size();
    v.clear();
    resize(rows,cols);
}
