#include "StEvtHddr.h"
#include <stdio.h>
#include <time.h>
#include <assert.h>

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
  SetParent(0);
  SetName(hddr.GetName());
  SetTitle(hddr.GetTitle());
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
   struct tm *tp;
   tp            = (tm*)gmtime((time_t*)&ut);
   UInt_t year   = tp->tm_year;
   if (year < 1900) year +=1900;
   UInt_t month  = tp->tm_mon + 1;
   UInt_t day    = tp->tm_mday;
   UInt_t hour   = tp->tm_hour;
   UInt_t min    = tp->tm_min;
   UInt_t sec    = tp->tm_sec;
   mEventTime.Set(year,month,day,hour,min,sec);
}
//_____________________________________________________________________________
  void StEvtHddr::Print()
{
  printf("\n *********** Event Info **********************\n");
  printf("\tRun: \t%5d  Event %5d  TimeStamp %8d.%6d  Bunch %u\n"
         ,mRunNumber,mEventNumber,mEventTime.GetDate(),mEventTime.GetTime()
         ,mBunchCrossingNumber);
  printf("\t EvtSize: %d \tInpTrig: %4x OutTrig: %4x  \n"
         ,mEventSize,mInputTriggerMask,mTriggerMask);
  printf("\n *********************************************\n\n");
}
