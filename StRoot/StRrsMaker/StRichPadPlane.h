/*****************************************************************
 * $Id: StRichPadPlane.h,v 1.8 2000/04/26 18:58:03 lasiuk Exp $   
 *   StRichPadPlane is a special container specialized to store 
 *   RICH Raw Data. 
 *
 * $Log: StRichPadPlane.h,v $
 * Revision 1.8  2000/04/26 18:58:03  lasiuk
 * change list to vector for mc info storage.
 * allow change of allocation methodology
 *
 * Revision 1.7  2000/04/05 16:03:04  lasiuk
 * remove size_type type definition.  Clash on SUN
 *
 * Revision 1.6  2000/02/14 20:54:03  lasiuk
 * : in class definition
 *
 * Revision 1.5  2000/02/14 01:11:47  lasiuk
 * alter StRichID for MC storage
 *
 * Revision 1.4  2000/02/08 16:36:45  lasiuk
 * Bring into line with HP
 *
 * Revision 1.3  2000/01/26 23:39:20  lasiuk
 * Forward declaration of classes to bypass CINT evaluation
 * comment the list data member in StRichID
 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 *   revision history:
 *     - 8/23/1999 created the type, tested          Alexandre Nevski.
 *
 ********************************************************************/
#ifndef ST_RICH_PADPLANE_H
#define ST_RICH_PADPLANE_H

#ifdef __ROOT__
#include "TObject.h"
#endif

#include <iostream.h>
//#include <list>
#include <vector>
//#include <deque>

#ifndef ST_NO_NAMESPACES
//using std::list;
using std::vector;
//using std::deque;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichEnumeratedTypes.h"

struct StRichID {
    StRichID()
	:  mHitID(0), mG_ID(0), mTrackp(0), mAmount(0), mSignalType(eUnknown) {/* nopt*/ }
	
    StRichID(int id, int gid, int track, int q =-1, StRichSignalType signalType=eUnknown)
	:  mHitID(id), mG_ID(gid), mTrackp(track), mAmount(q), mSignalType(signalType) {/* nopt*/ }

    int operator==(const StRichID& anID) const;

    int mHitID;
    int mG_ID;
    int mTrackp;
    int mAmount;
    StRichSignalType mSignalType;
};

inline int StRichID::operator==(const StRichID& anID) const
{
    return ((mG_ID == anID.mG_ID) &&
	    (mTrackp == anID.mTrackp) &&
	    (mSignalType == anID.mSignalType));
}

ostream& operator<<(ostream& os, const StRichID& id);


#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StRichID>        anIDList;
#else
typedef vector<StRichID, allocator<StRichID> >           anIDList;
#endif

class StRichPad {
public:
    StRichPad()	: signal(0) {/* nopt */}
    ~StRichPad() {}
    double signal;
    
    // the quadrants of the pad plane
    anIDList IDs; //!
};    


//typedef unsigned int size_type;
typedef StRichPad pad_type;
typedef StRichID id_type;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StRichPad>     aDetectorRow;
typedef vector<aDetectorRow>  aPadPlane;
#else
typedef vector<StRichPad, allocator<StRichPad> >       aDetectorRow;
typedef vector<aDetectorRow, allocator<aDetectorRow> > aPadPlane;
#endif

typedef aDetectorRow row_type;
typedef aPadPlane impl_type;
typedef anIDList::iterator id_iter;
typedef anIDList::const_iterator const_id_iter;

typedef impl_type::iterator row_iter;
typedef impl_type::const_iterator const_row_iter;
typedef row_type::iterator pad_iter;
typedef row_type::const_iterator const_pad_iter;
	

class StRichPadPlane
#ifdef __ROOT__
    : public TObject
#endif
{
public:
    StRichPadPlane();
    StRichPadPlane(size_t rows, size_t cols);  
	
    ~StRichPadPlane();
	
    row_type& operator[](int n) { return v[n]; }
	
    const_row_iter begin() const { return v.begin(); }
    row_iter begin()  {return v.begin(); }
    const_row_iter end() const { return v.end(); }
    row_iter end() { return v.end(); }
	
    void resize(size_t rows, size_t cols);
    
    void clear();
    
    size_t row_size() { return v.size(); }
    size_t col_size() { return v.begin()->size(); }

public:
    aPadPlane v;

};
#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
