// Author: Victor Perev   08/04/01


#ifndef ROOT_TUnixTime
#define ROOT_TUnixTime


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TUnixTime                                                          //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TString.h"
class TDatime;
struct tm;
class TUnixTime
{
public:
   TUnixTime(UInt_t utime = 0);
   TUnixTime(Int_t date,Int_t time,int gmt);
   TUnixTime(const TDatime &tdt,int gmt);
   virtual ~TUnixTime(){;}
   UInt_t operator()() const		{ return fUTime;}
   TUnixTime &operator =(UInt_t ut) 	{ fUTime =ut ; return *this;}
   TUnixTime &operator+=(Int_t sec) 	{ fUTime+=sec; return *this;}
   UInt_t GetUTime()			{ return fUTime;}
   void   GetLTime(Int_t &idate, Int_t &itime);
   void   GetGTime(Int_t &idate, Int_t &itime);
   TString GetLString();   
   TString GetGString();   

   void SetUTime(UInt_t utime){ fUTime=utime;}
   void SetLTime(Int_t idate, Int_t itime);
   void SetGTime(Int_t idate, Int_t itime);
   void SetLTime(const TDatime &loc);
   void SetGTime(const TDatime &gmt);
static UInt_t Convert(const TDatime &dt,int gmt);
private:
   void SetGTime(const struct tm *gt);
   void SetLTime(const struct tm *gt);

//		Data members
   UInt_t fUTime; 
   
   ClassDef(TUnixTime,1)

};
#endif //ROOT_TUnixTime
