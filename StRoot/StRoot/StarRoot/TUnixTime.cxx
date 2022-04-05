/***************************************************************************
 *
 * $Id: TUnixTime.cxx,v 1.4 2013/02/20 01:59:13 perev Exp $
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
#include "TDatime.h"


ClassImp(TUnixTime)
//______________________________________________________________________________
TUnixTime::TUnixTime(ULong_t utime)
{
  fUTime = utime;
  if (!fUTime) fUTime = time(0);
}
//______________________________________________________________________________
TUnixTime::TUnixTime(Int_t date,Int_t time,int gmt)
{
  if (gmt) SetGTime(date,time);
  else     SetLTime(date,time);
}
//______________________________________________________________________________
TUnixTime::TUnixTime(const TDatime &tdt,int gmt)
{
   if (gmt) {SetGTime(tdt.GetDate(),tdt.GetTime());}
   else     {SetLTime(tdt.GetDate(),tdt.GetTime());}
}
//______________________________________________________________________________
void TUnixTime::SetGTime(const struct tm *tm_gmt)
{
  int dif;
  time_t ugmt;
  struct tm tm_test;
  tm_test = *tm_gmt;
  tm_test.tm_isdst=-1;
  ugmt = mktime(&tm_test)-timezone;
  for (int iter=0;iter<24;iter++) {
    tm_test = *gmtime(&ugmt);
    tm_test.tm_isdst=-1;
    dif = tm_test.tm_year - tm_gmt->tm_year;
    if (dif) 	goto UPD;
    dif = tm_test.tm_mon  - tm_gmt->tm_mon ;
    if (dif)  	goto UPD;
    dif = tm_test.tm_mday - tm_gmt->tm_mday;
    if (dif)  	goto UPD;
    dif = tm_test.tm_hour - tm_gmt->tm_hour;
    if (!dif) 	goto OK;
UPD: if (dif<0) ugmt+=3600;    
     else       ugmt-=3600;
  }
  assert(0);
OK: fUTime = ugmt;

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
  gt->tm_isdst =-1;
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
//______________________________________________________________________________
void TUnixTime::SetLTime(const TDatime &loc)
{
  SetLTime(loc.GetDate(), loc.GetTime());
}
//______________________________________________________________________________
void TUnixTime::SetGTime(const TDatime &gmt)
{
  SetGTime(gmt.GetDate(), gmt.GetTime());
}
//______________________________________________________________________________
ULong_t TUnixTime::Convert(const TDatime &dt,int gmt)
{
   TUnixTime ux;
   if (gmt) {ux.SetGTime(dt);}
   else     {ux.SetLTime(dt);}
   return ux.GetUTime();
}
