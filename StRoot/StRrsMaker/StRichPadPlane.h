/*****************************************************************
 * $Id: StRichPadPlane.h,v 1.2 2000/01/25 22:02:21 lasiuk Exp $   
 *   StRichPadPlane is a special container specialized to store 
 *   RICH Raw Data. 
 *
 * $Log: StRichPadPlane.h,v $
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
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

#include "TObject.h"

#endif
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
using std::list;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
	:  G_ID(0), amount(0.0) {/* nopt*/ }
struct StRichID {
    StRichID(int i,double d =-1.0)
	:  G_ID(i), amount(d) {/* nopt*/ }
	
    int G_ID;
    double amount;
    int mG_ID;
    int mTrackp;
    double mAmount;
};


#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StRichID>        anIDList;
#else
typedef list<StRichID, allocator<StRichID> >           anIDList;
#endif
    anIDList IDs;
struct StRichPad {
    StRichPad()
	: signal(0), IDs() {/* nopt */}
    double signal;
    
    // the quadrants of the pad plane
    anIDList IDs; //!
};    


typedef unsigned int size_type;
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
	
class StRichPadPlane {
    : public TObject
#endif
{
public:
    StRichPadPlane();
    StRichPadPlane(size_type rows, size_type cols);  
	
    ~StRichPadPlane();
	
    row_type& operator[](int n) { return v[n]; }
	
    const_row_iter begin() const { return v.begin(); }
    row_iter begin()  {return v.begin(); }
    const_row_iter end() const { return v.end(); }
    row_iter end() { return v.end(); }
	
    void resize(size_type rows, size_type cols);
    
    void clear();
    
    size_type row_size() { return v.size(); }
    size_type col_size() { return v.begin()->size(); }

public:
    aPadPlane v;

};
#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
