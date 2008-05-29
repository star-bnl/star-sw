#ifndef StSortTofRawData_h
#define StSortTofRawData_h

class StEvent;
class StTofRawData;
class StTofCollection;
class StTofRawDataCollection;
class StTofrDaqMap;

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
  int triggertime;
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
  static const UInt_t mNTRAY = 122;
  tofRawHitVector mRawHitVec[mNTRAY];
  
  static const Int_t mNTOF = 192;
  Int_t mTDIGLeChan2WestPMT[mNTOF], mTDIGTeChan2WestPMT[mNTOF]; //
  Int_t mTDIGLeChan2EastPMT[mNTOF], mTDIGTeChan2EastPMT[mNTOF]; //
  
  //--added by Zebo 
  Float_t mTimeWindow[mNTRAY][2];  
  //--end
 public:
  StSortTofRawData();
  StSortTofRawData(StTofCollection*);
  StSortTofRawData(StTofCollection*, StTofrDaqMap*);
  ~StSortTofRawData();
  
  void Init(StTofCollection*);
  void InitRun8(StTofCollection*);
  void Reset();
  
  IntVec GetValidChannel();
  IntVec GetLeadingTdc(int channel);
  IntVec GetTrailingTdc(int channel);

  IntVec GetValidChannel(int tray);
  IntVec GetLeadingTdc(int tray, int channel);
  IntVec GetTrailingTdc(int tray, int channel);
  //--added by Zebo
  IntVec GetLeadingTdc(int tray, int channel, bool triggerevent);
  IntVec GetTrailingTdc(int tray, int channel, bool triggerevent);
  //--end
  Int_t GetTriggerTime(int tray, int channel);
  
  void SetVPDMap(StTofrDaqMap* daqMap);

  ClassDef(StSortTofRawData,4)
};
#endif


