#ifndef StSortTofRawData_h
#define StSortTofRawData_h

class StEvent;
class StTofRawData;
class StTofCollection;
class StTofRawDataCollection;

#include "StObject.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
//#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<Int_t>  IntVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
#endif

struct TOFRawHit {
  int tray;
  int channel;
  IntVec leadingTdc;
  IntVec trailingTdc;
};

#ifndef ST_NO_TEMPLATE_DEF_ARGS
  typedef vector<TOFRawHit> tofRawHitVector;
#else
  typedef vector<TOFRawHit,allocator<TOFRawHit>> tofRawHitVector;
#endif
  typedef vector<TOFRawHit>::iterator tofRawHitVectorIter;

class StSortTofRawData : public StObject {
 private:
  tofRawHitVector mRawHitVec;

 public:
  StSortTofRawData();
  StSortTofRawData(StTofCollection*);
  ~StSortTofRawData();
  
  void Init(StTofCollection*);
  void Reset();
  
  IntVec GetValidChannel();
  IntVec GetLeadingTdc(int channel);
  IntVec GetTrailingTdc(int channel);

  IntVec GetValidChannel(int tray);
  IntVec GetLeadingTdc(int tray, int channel);
  IntVec GetTrailingTdc(int tray, int channel);

  ClassDef(StSortTofRawData,2)
};
#endif


