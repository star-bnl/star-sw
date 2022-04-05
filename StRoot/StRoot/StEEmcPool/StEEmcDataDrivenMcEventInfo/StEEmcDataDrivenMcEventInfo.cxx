//
// Ilya Selyuzhenkov <ilya.selyuzhenkov@gmail.com>
// Indiana University Cyclotron Facility
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
//

#include "StEEmcDataDrivenMcEventInfo.h"

ClassImp(StEEmcDataDrivenMcEventInfo);

StEEmcDataDrivenMcEventInfo::StEEmcDataDrivenMcEventInfo()
{
  InitArrays();
  Clear();
}

StEEmcDataDrivenMcEventInfo::~StEEmcDataDrivenMcEventInfo(){}

void StEEmcDataDrivenMcEventInfo::Clear(Option_t* options)
{
  mRunId   = -999;
  mEventId = -999;
  mReplaceInfo->Clear();
}

void StEEmcDataDrivenMcEventInfo::InitArrays()
{
  mReplaceInfo = new TClonesArray("StEEmcDataDrivenMcReplaceInfo",100);
}

StEEmcDataDrivenMcReplaceInfo *StEEmcDataDrivenMcEventInfo::newReplaceInfo()
{
  TClonesArray &replaceInfos = *mReplaceInfo;
  return new (replaceInfos[replaceInfos.GetEntriesFast()]) StEEmcDataDrivenMcReplaceInfo;
}
