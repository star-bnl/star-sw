/******************************************************************
 *  $Id: StRichPadPlane.cxx,v 1.5 2000/05/17 22:27:39 lasiuk Exp $
 *
 * Description:
 *   StRichPadPlane is a special container specialized to store 
 *   RICH Raw Data. 
 ********************************************************************
 *  $Log: StRichPadPlane.cxx,v $
 *  Revision 1.5  2000/05/17 22:27:39  lasiuk
 *  use charge()
 *  use mHitID in comparator for StRichID
 *
 *  Revision 1.4  2000/04/26 18:58:03  lasiuk
 *  change list to vector for mc info storage.
 *  allow change of allocation methodology
 *
 *  Revision 1.3  2000/04/14 22:38:09  lasiuk
 *  add print diagnostic for crash
 *  extra careful on clearing the dataset
 *
 *  Revision 1.2  2000/04/05 16:03:00  lasiuk
 *  remove size_type type definition.  Clash on SUN
 *
 *  Revision 1.1  2000/01/18 21:32:03  lasiuk
 *  Initial Revision
 *
 ********************************************************************/
#define NORMAL_ALLOCATION

#include "StRichPadPlane.h"
#include "StGlobals.hh"

ostream& operator<<(ostream& os, const StRichID& id)
{
    return (os << "id.mHitID: " << id.mHitID << " GID: " << id.mG_ID << " track: " << id.mTrackp
	       << " q: " << id.mCharge << " type: " << static_cast<int>(id.mSignalType));
}

StRichPadPlane::StRichPadPlane() { /*nopt*/ }
    
StRichPadPlane::StRichPadPlane(size_t rows, size_t cols)
{
    //cout << "StRichPadPlane::StRichPadPlane() " << endl;

//  PR(rows);
//  PR(cols);

    //
    // How a normal person would do this
    //
#ifdef NORMAL_ALLOCATION
     StRichPad tmp;
     cout << "NORMAL_ALLOCATION" << endl;
     v.resize(rows);
//      PR(v.size());
//      PR(v.capacity());
     for (unsigned int i=0; i<rows; i++ ) {
	 v[i].resize(cols,tmp);
	 if(v[i].size() != cols) {
	     cout << "StRichPadPlane::StRichPadPlane()\n";
	     cout << "\tv[i].size()= " << v[i].size() << "\n";
	     cout << "\tAborting...Error in allocation" << endl;
	     abort();
	 }
     }
#else
     unsigned int ii, jj;
     cout << "NOT NORMAL_ALLOCATION" << endl;

     aDetectorRow aRow;
     for(ii=0; ii<cols; ii++) {
	 aRow.push_back(StRichPad());
     }
     
     PR(aRow.size());
     
     for(jj=0; jj<rows; jj++) {
	v.push_back(aRow);
     }
#endif

//      PR(v.size());
//      PR(v.begin()->size());
//      PR(this->row_size());
//      PR(this->col_size());
}

StRichPadPlane::~StRichPadPlane() { /* nopt */ }
    
void StRichPadPlane::resize(size_t rows, size_t cols)
{
//      cout << "StRichPadPlane::resize()" << endl;
//      PR(rows);
//      PR(cols);
//      PR(v.size());
#ifdef NORMAL_ALLOCATION
//      cout << "NORMAL_ALLOCATION" << endl;
    StRichPad tmp;
    if (v.size() != rows)
	v.resize(rows);
//     PR(v.size());
    for ( unsigned int i = 0; i < v.size(); i++ ) {
	v[i].resize(cols,tmp);
    }
#else
    cout << "NOT NORMAL_ALLOCATION" << endl;
    if (v.size() != rows)
	v.resize(rows);
    PR(v.size());
    for ( unsigned int i = 0; i < v.size(); i++ ) {
 	for(unsigned int j=0; j<cols; j++) {
 	    v[i].push_back(StRichPad());
 	}
    }
#endif
}

void StRichPadPlane::clear()
{
    //cout << "StRichPadPlane::clear()" << endl;
    unsigned int rows = this->row_size();
    unsigned int cols = this->col_size();
//      PR(rows);
//      PR(cols);
//      PR(v.size());
//      for(int ii=0; ii<v.size(); ii++) {
//  	cout << "ii -> size " << ii << " " << v[ii].size() << endl;
//  	v[ii].clear();
//      }
	
    v.clear();
    this->resize(rows,cols);
}
