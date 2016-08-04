#include "StMuMtdHeader.h"
#include "StEvent/StEvent.h"
#include "StEvent/StMtdHeader.h"

ClassImp(StMuMtdHeader)

StMuMtdHeader::StMuMtdHeader() : mFiberHeader1(0), mFiberHeader2(0), mFiberTriggerWord1(0), mFiberTriggerWord2(0),
  mTriggerTime1(0), mTriggerTime2(0), mShouldHaveRejectEvent(-1), mTpcSectorMask(0)
{
  // default constructor
}

StMuMtdHeader::StMuMtdHeader(const StMtdHeader *header) : StMuMtdHeader()
{
  if (header){
    for(int i=0; i<header->MAXFIBER; i++){
      if(i==0) {
	mFiberHeader1 = header->fiberHeader(i);
	mFiberTriggerWord1 = header->fiberTriggerWord(i);
	mTriggerTime1 = header->triggerTime(i); 		
      }
      else if(i==1){
	mFiberHeader2 = header->fiberHeader(i);
	mFiberTriggerWord2 = header->fiberTriggerWord(i);
	mTriggerTime2 = header->triggerTime(i); 		
      }
    }
    mShouldHaveRejectEvent = header->shouldHaveRejectEvent();
    mTpcSectorMask = header->tpcSectorMask();
  }	
}

short StMuMtdHeader::fiberHeader(int fiberId) const {
  if (fiberId==1) return mFiberHeader1;
  else if (fiberId==2) return mFiberHeader2;
  else return 0;
}

unsigned int StMuMtdHeader::fiberTriggerWord(int fiberId) const {
  if (fiberId==1) return mFiberTriggerWord1;
  else if (fiberId==2) return  mFiberTriggerWord2;
  else return 0;
}

unsigned int StMuMtdHeader::triggerTime(int fiberId) const {
  if (fiberId==1) return mTriggerTime1;
  else if (fiberId==2) return  mTriggerTime2;
  else return 0;
}

int StMuMtdHeader::shouldHaveRejectEvent() const {
  return mShouldHaveRejectEvent;
}

unsigned int StMuMtdHeader::tpcSectorMask() const {
  return mTpcSectorMask;
}
