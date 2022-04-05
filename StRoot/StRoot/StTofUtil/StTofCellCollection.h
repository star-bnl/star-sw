/****************************************************************
 * $Id: StTofCellCollection.h,v 1.2 2015/07/28 22:55:43 smirnovd Exp $
 *****************************************************************
 * Description: Local TOF cells collection
 *
 *****************************************************************
 * $Log: StTofCellCollection.h,v $
 * Revision 1.2  2015/07/28 22:55:43  smirnovd
 * Added cstddef C++ header defining size_t type
 *
 * Revision 1.1  2003/08/06 22:59:36  geurts
 * First Release
 *  - used by TOF MatchMakers
 *
 *
 ****************************************************************/
#ifndef ST_TOF_CELL_COLLECTION_H
#define ST_TOF_CELL_COLLECTION_H

#include <cstddef>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

class StTofCell;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
typedef vector<StTofCell*> cellVector;
#else
typedef vector<StTofCell*, allocator<StTofCell*> > cellVector;
#endif

class StTofCellCollection {
public:
    StTofCellCollection();
    virtual ~StTofCellCollection();

    bool        push_back(StTofCell* cell);
    size_t      size()  const;
    StTofCell*  front() const;
    StTofCell*  back()  const;
    StTofCell*  getCell(size_t index) const;
    void        clear();

 private:
    cellVector         mCellVector;
};
#endif
