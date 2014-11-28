/****************************************************************  
 * $Id: StTofDataCollection.h,v 1.1 2003/08/08 00:18:25 geurts Exp $
 *****************************************************************
 * Author: Bill Llope
 * Description: Local TOF raw data collection
 *****************************************************************
 * $Log: StTofDataCollection.h,v $
 * Revision 1.1  2003/08/08 00:18:25  geurts
 * moved from StTofMaker to StTofUtil
 *
 *
 ****************************************************************/
#ifndef ST_TOF_DATA_COLLECTION_H
#define ST_TOF_DATA_COLLECTION_H

#include <vector>
#include <cstring>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

//#include "StTofData.h"
class StTofData;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofData*> dataVector;
#else
typedef vector<StTofData*, allocator<StTofData*> > dataVector;
#endif

class StTofDataCollection {
public:
    StTofDataCollection();
    virtual ~StTofDataCollection();
    bool        push_back(StTofData* chan);
    size_t      size()  const;
    StTofData*  front() const;
    StTofData*  back()  const;
    StTofData*  getData(size_t index) const;
    void clear();    
    
private:
    dataVector         mDataVector;
};
#endif
