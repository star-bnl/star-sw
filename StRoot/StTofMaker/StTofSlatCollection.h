/****************************************************************
 * $Id: StTofSlatCollection.h,v 1.3 2001/09/28 18:40:03 llope Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Local TOF slats collection
 *
 *****************************************************************
 *
 * $Log: StTofSlatCollection.h,v $
 * Revision 1.3  2001/09/28 18:40:03  llope
 * first release
 *
 * Revision 1.1  2001/04/24 20:27:38  wzhang
 * First release
 *
 *
 ****************************************************************/

#ifndef ST_TOF_SLAT_COLLECTION_H
#define ST_TOF_SLAT_COLLECTION_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif


#include "StTofSlat.h"

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofSlat*> slatVector;
#else
typedef vector<StTofSlat*, allocator<StTofSlat*> > slatVector;
#endif

class StTofSlatCollection {
public:
    StTofSlatCollection();
    virtual ~StTofSlatCollection();

    // Do not use default
//    StTofSlatCollection(const StTofSlatCollection& old);
//    StTofSlatCollection& operator=(const StTofSlatCollection& old);

    bool        push_back(StTofSlat* slat);
    size_t      size()  const;
    StTofSlat*  front() const;
    StTofSlat*  back()  const;
    StTofSlat*  getSlat(size_t index) const;

    void clear();
    
    
private:

    slatVector         mSlatVector;
};

#endif /* _TOF */
