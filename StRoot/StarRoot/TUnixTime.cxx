/***************************************************************************
 *
 * $Id: TUnixTime.cxx,v 1.1 2001/09/10 23:53:20 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "TUnixTime.h"


ClassImp(TUnixTime)
//______________________________________________________________________________
TUnixTime::TUnixTime(UInt_t utime)
{
  fUTime = utime;
  if (!fUTime) fUTime = time(0);
}
//______________________________________________________________________________
void TUnixTime::SetGTime(const struct tm *gt)
{
  time_t ut,ug;
  struct tm gtt;
  gtt = *gt;
  gtt.tm_isdst=-1;
  ut =  mktime(&gtt);
  gtt = *gmtime(&ut);
  gtt.tm_isdst=-1;
  ug = mktime(&gtt);
  ug = ut + (ut-ug);
  fUTime = ug;
  gtt = *gmtime(&ug);
  assert(gtt.tm_year==gt->tm_year);
  assert(gtt.tm_mon ==gt->tm_mon );
  assert(gtt.tm_mday==gt->tm_mday);
  assert(gtt.tm_hour==gt->tm_hour);

}
//______________________________________________________________________________
void TUnixTime::SetLTime(const struct tm *lt)
{
  struct tm ltt;
  ltt = *lt;
  fUTime = mktime(&ltt);
}
//______________________________________________________________________________
static void DateTime2tm(struct tm *gt,Int_t idate, Int_t itime)
{
  time_t ul = time(0);
  *gt = *localtime(&ul);
  if (idate < 19000000) idate +=19000000;
  if (idate < 19500000) idate += 1000000;
  gt->tm_year= ((idate/10000)-1900);        
  gt->tm_mon = ((idate/100)%100) - 1; 
  gt->tm_mday= ((idate    )%100);
  gt->tm_hour=  itime/10000;
  gt->tm_min = (itime/100 )%100;
  gt->tm_sec = (itime     )%100;
}

//______________________________________________________________________________
static void tm2DateTime(Int_t &idate, Int_t &itime,const struct tm *gt)
{
  if (idate < 19000000) idate +=19000000;
  if (idate < 19500000) idate += 1000000;
  idate  = (gt->tm_year+1900)*10000 + (gt->tm_mon+1)*100 + gt->tm_mday;
  itime  =  gt->tm_hour*10000 + gt->tm_min*100 + gt->tm_sec;
}

//______________________________________________________________________________
void TUnixTime::SetGTime(Int_t idate, Int_t itime)
{
  struct tm gt;
  DateTime2tm(&gt,idate,itime);
  SetGTime(&gt);

}
//______________________________________________________________________________
void TUnixTime::SetLTime(Int_t idate, Int_t itime)
{
  struct tm gt;
  DateTime2tm(&gt,idate,itime);
  SetLTime(&gt);

}
//______________________________________________________________________________
void TUnixTime::GetGTime(Int_t &idate, Int_t &itime)
{
  struct tm gt;
  gt = *gmtime((time_t*)&fUTime);
  tm2DateTime(idate,itime,&gt);

}
//______________________________________________________________________________
void TUnixTime::GetLTime(Int_t &idate, Int_t &itime)
{
  struct tm gt;
  gt = *localtime((time_t*)&fUTime);
  tm2DateTime(idate,itime,&gt);

}
//______________________________________________________________________________
TString TUnixTime::GetLString()
{
  TString ts(ctime((time_t*)&fUTime));
  return ts;
}  
//______________________________________________________________________________
TString TUnixTime::GetGString()
{
  TString ts(asctime(gmtime((time_t*)&fUTime)));
  return ts;
}  

