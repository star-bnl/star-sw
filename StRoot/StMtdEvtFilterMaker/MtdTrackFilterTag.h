#ifndef __MtdTrackFilterTag_st__
#define __MtdTrackFilterTag_st__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
//
// MTD event filtering tags
//

struct MtdTrackFilterTag_st {
    long tpcSectors;
    long isRejectEvent;
    long shouldHaveRejectEvent;
};
class St_MtdTrackFilterTag : public TTable {
 public:
  ClassDefTable(St_MtdTrackFilterTag,MtdTrackFilterTag_st)
  ClassDef(St_MtdTrackFilterTag,1) //C++ container for chain/makers status 
};

#endif /* __MtdTrackFilterTag_st__ */
