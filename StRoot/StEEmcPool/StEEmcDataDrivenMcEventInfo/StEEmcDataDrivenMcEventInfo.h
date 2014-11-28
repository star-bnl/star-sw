//
// Ilya Selyuzhenkov <ilya.selyuzhenkov@gmail.com>
// Indiana University Cyclotron Facility
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
//

#ifndef __StEEmcDataDrivenMcEventInfo_h__
#define __StEEmcDataDrivenMcEventInfo_h__

#include "TVector3.h"
#include "TClonesArray.h"

#include "StEEmcDataDrivenMcReplaceInfo.h"

class StEEmcDataDrivenMcEventInfo : public TObject {
public:
  StEEmcDataDrivenMcEventInfo();
  virtual ~StEEmcDataDrivenMcEventInfo();

  void Clear(Option_t* options = "");
  StEEmcDataDrivenMcReplaceInfo* newReplaceInfo();
  int NumberOfReplacements() const { return mReplaceInfo->GetEntriesFast(); }
  StEEmcDataDrivenMcReplaceInfo* ReplaceInfo(int i) const { return (StEEmcDataDrivenMcReplaceInfo*)mReplaceInfo->At(i); }
  int runId() const {return mRunId;}
  int eventId() const {return mEventId;}
  TString fileName() const {return mFileName;}

  void SetRunId(int run) { mRunId = run; }
  void SetEventId(int event) { mEventId = event; }
  void SetFileName(const char* fileName){ mFileName = fileName; }

protected:
  int mRunId;
  int mEventId;
  TString mFileName;

private:
  void InitArrays();

  TClonesArray* mReplaceInfo;

  ClassDef(StEEmcDataDrivenMcEventInfo,1);
};

#endif
