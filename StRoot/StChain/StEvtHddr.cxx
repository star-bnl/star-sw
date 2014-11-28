#include <stdio.h>
#include <time.h>
#include <assert.h>
#include "StEvtHddr.h"
#include "StarRoot/TUnixTime.h"
#include "StMessMgr.h"

ClassImp(StEvtHddr)
//_____________________________________________________________________________
StEvtHddr::StEvtHddr(TDataSet *parent):TDataSet("EvtHddr",parent)
{
  SetDateTime(20330101,0);
  SetEventType("NONE");
  memset(&mRunNumber,0,(char*)(&mEventNumber)-(char*)&mRunNumber);
  mRunNumber=-1;mOldRunNumber=-2;mEventNumber=-1;
}
//_____________________________________________________________________________
StEvtHddr &StEvtHddr::operator=(const StEvtHddr &hddr)
{
  if (!GetName() [0]) SetName (hddr.GetName ());
  if (!GetTitle()[0]) SetTitle(hddr.GetTitle());
  memcpy(&mRunNumber,&hddr.mRunNumber,(char*)((&mEventNumber)+1)-(char*)&mRunNumber);
  mEventTime = hddr.mEventTime;
  mProdTime  = hddr.mProdTime;
  mEventType = hddr.mEventType;
  return *this;
}
//_____________________________________________________________________________
void StEvtHddr::FillTag(EvtHddr_st *tag)
{

  assert((char*)&mEventNumber     -(char*)&mRunNumber
       ==(char*)&tag->mEventNumber-(char*)&tag->mRunNumber);

  memcpy(tag,&mRunNumber,(char*)(&mEventNumber+1)-(char*)&mRunNumber);
  tag->mEventTime = mEventTime.GetDate() + mEventTime.GetTime()/1000000.;
  tag->mProdTime  =  mProdTime.GetDate() +  mProdTime.GetTime()/1000000. ;
  tag->mEventType[0] = 0;
  strncat(tag->mEventType,mEventType,15);
}
//_____________________________________________________________________________
  void StEvtHddr::SetGMTime(UInt_t ut)
{
   TUnixTime unixTime(ut);
   Int_t dat=0,tim=0;
   unixTime.GetGTime(dat,tim);
   mEventTime.Set(dat,tim);
}
//_____________________________________________________________________________
  void  StEvtHddr::SetProdDateTime(UInt_t ut)
{
   TUnixTime unixTime(ut);
   Int_t dat=0,tim=0;
   unixTime.GetGTime(dat,tim);
   mProdTime.Set(dat,tim);
}
//_____________________________________________________________________________
  UInt_t StEvtHddr::GetUTime() 	  const 
{
    TUnixTime unixTime;
    unixTime.SetGTime(mEventTime.GetDate(),mEventTime.GetTime());
    return unixTime.GetUTime();
}
//_____________________________________________________________________________
  void StEvtHddr::Print(Option_t *option) const
{
   LOG_INFO << Form 
  /* printf*/ ("*********** Event Info **********************")
         << endm;
   LOG_INFO << Form 
  /* printf*/ ("\tRun: \t%5d  Event %5d  TimeStamp %8d.%6d  Bunch %d:%d"
         ,mRunNumber,mEventNumber,mEventTime.GetDate(),mEventTime.GetTime()
         ,mBunchCrossingNumber[0],mBunchCrossingNumber[1])
         << endm;
  LOG_INFO << Form 
  /* printf*/ ("\t EvtSize: %d \tInpTrig: %4x OutTrig: %4x"
         ,mEventSize,mInputTriggerMask,mTriggerMask)
        << endm;
  LOG_INFO << Form 
  /* printf*/  ("*********************************************")
        << endm;
}
