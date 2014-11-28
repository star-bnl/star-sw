/****************************************************************
 * $Id: StTofSlatCollection.h,v 1.1 2003/08/08 00:18:26 geurts Exp $
 *****************************************************************
 * Author: Wei-Ming Zhang, April 2001
 * Description: Local TOF slats collection
 *
 *****************************************************************
 * $Log: StTofSlatCollection.h,v $
 * Revision 1.1  2003/08/08 00:18:26  geurts
 * moved from StTofMaker to StTofUtil
 *
 *
 ****************************************************************/
#ifndef ST_TOF_SLAT_COLLECTION_H
#define ST_TOF_SLAT_COLLECTION_H

#include <vector>
#include <cstring>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

class StTofSlat;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofSlat*> slatVector;
#else
typedef vector<StTofSlat*, allocator<StTofSlat*> > slatVector;
#endif

class StTofSlatCollection {
public:
    StTofSlatCollection();
    virtual ~StTofSlatCollection();

    bool        push_back(StTofSlat* slat);
    size_t      size()  const;
    StTofSlat*  front() const;
    StTofSlat*  back()  const;
    StTofSlat*  getSlat(size_t index) const;
    void        clear();
    
 private:
    slatVector         mSlatVector;
};
#endif
