/*!
 * \class TMemStat 
 * \author Victor Perev, Jul 2000
 *
 *  Simplified version of TMemStatoryInfo class of Thomas Ullrich
 *  Measurement of used heap memory and total program size
 *
 *  Note: on Solaris should be linked with -lmalloc
 */
/***************************************************************************
 *
 * $Id: TMemStat.h,v 1.2 2003/10/25 02:58:51 jeromel Exp $
 *
 * Author: Victor Perev, Jul 2000
 **************************************************************************/
#ifndef TMemStat_h
#define TMemStat_h
#include "TNamed.h"

class TList;

class TMemStat :public TNamed {
public:
    TMemStat(const char *name=0);
   ~TMemStat();
   void Start();
   void Stop();
   virtual void   Print(const char *tit="") const;

   //static methods

   static  Double_t Used();			/*!< Used heap memory in MB    */
   static  Double_t ProgSize();			/*!< Program size     in MB    */
   static  void     PrintMem(const char *tit);	/*!< Prints current memory     */
   static  void     Summary();			/*!< Prints usage summary      */

 private:
   Double_t fLast;
   Double_t fMin;
   Double_t fAver;
   Double_t fMax;
   Double_t fRms;
   Int_t    fTally;

   static Double_t fgUsed;
   static TList    *fgList;

   ClassDef(TMemStat,0)
};

#endif
