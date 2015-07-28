/****************************************************************  
 * $Id: StTofRawDataCollection.h,v 1.2 2015/07/28 22:55:44 smirnovd Exp $
 *****************************************************************
 * Author: Xin Dong
 * Description: Local TOF raw data collection - only valid hits
 *****************************************************************
 * $Log: StTofRawDataCollection.h,v $
 * Revision 1.2  2015/07/28 22:55:44  smirnovd
 * Added cstddef C++ header defining size_t type
 *
 * Revision 1.1  2005/04/12 17:29:29  dongx
 * first release, a new data format in StEvent for year 5
 *
 *
 ****************************************************************/
#ifndef ST_TOF_RAWDATA_COLLECTION_H
#define ST_TOF_RAWDATA_COLLECTION_H

#include <cstddef>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

//#include "StTofRawData.h"
class StTofRawData;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofRawData*> rawdataVector;
#else
typedef vector<StTofRawData*, allocator<StTofRawData*> > rawdataVector;
#endif

class StTofRawDataCollection {
public:
    StTofRawDataCollection();
    virtual ~StTofRawDataCollection();
    bool        push_back(StTofRawData* chan);
    size_t      size()  const;
    StTofRawData*  front() const;
    StTofRawData*  back()  const;
    StTofRawData*  getRawData(size_t index) const;
    void clear();    
    
private:
    rawdataVector         mDataVector;
};
#endif
