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

struct tm;
class TUnixTime
{
public:
   TUnixTime(UInt_t utime = 0);
   virtual ~TUnixTime(){;}
   UInt_t GetUTime(){ return fUTime;}
   void   GetLTime(Int_t &idate, Int_t &itime);
   void   GetGTime(Int_t &idate, Int_t &itime);
   TString GetLString();   
   TString GetGString();   

   void SetUTime(UInt_t utime){ fUTime=utime;}
   void SetLTime(Int_t idate, Int_t itime);
   void SetGTime(Int_t idate, Int_t itime);
private:
   void SetGTime(const struct tm *gt);
   void SetLTime(const struct tm *gt);

//		Data members
   UInt_t fUTime; 
   
   ClassDef(TUnixTime,1)

};
#endif //ROOT_TUnixTime
