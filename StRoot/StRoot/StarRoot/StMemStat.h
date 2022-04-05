/*!
 * \class StMemStat 
 * \author Victor Perev, Jul 2000
 *
 *  Simplified version of StMemStatoryInfo class of Thomas Ullrich
 *  Measurement of used heap memory and total program size
 *
 *  Note: on Solaris should be linked with -lmalloc
 */
/***************************************************************************
 *
 * $Id: StMemStat.h,v 1.6 2019/07/22 18:27:12 smirnovd Exp $
 *
 * Author: Victor Perev, Jul 2000
 **************************************************************************/
#ifndef StMemStat_h
#define StMemStat_h

#include <map>

#include "TNamed.h"

class TList;

class StMemStat :public TNamed {
public:
    StMemStat(const char *name=0);
   ~StMemStat();
   void Start();
   void Stop();
   virtual void   Print(const char *tit="") const;

   //static methods

   static  Double_t Used();			/*!< Used heap memory in MB    */
   static  Double_t Free();			/*!< Free heap memory in MB    */
   static  Double_t ProgSize();			/*!< Program size     in MB    */
   static  void     PrintMem(std::string prefix="");	/*!< Prints current memory     */

   /// Returns a string with memory utilization estimates for the current process
   static  std::string AsString(std::string prefix="");

   static  void     PM();	                /*!< Prints fast current heap  */
   static  void     Summary();			/*!< Prints usage summary      */
   static  void     doPs(std::string who, std::string where);

   /// Saves the values read from /proc/self/status at the time of a call in a text file.
   /// See StMemStat::ReadProcStatus() for parsed values
   static void SaveProcStatus(std::string callerId);

 private:
   Double_t fLast;
   Double_t fMin;
   Double_t fAver;
   Double_t fMax;
   Double_t fRms;
   Int_t    fTally;

   static Double_t fgUsed;
   static TList    *fgList;

   /// This type should be `std::unordered_map` but `rootcint` cannot deal with c++11
   typedef std::map<std::string, double> ProcStatusMap_t;
   typedef ProcStatusMap_t::value_type   ProcStatus_t;

   /// Maps values to the corresponding tokens read from /proc/pid/status
   static ProcStatusMap_t ReadProcStatus();

   ClassDef(StMemStat,0)
};

#endif
