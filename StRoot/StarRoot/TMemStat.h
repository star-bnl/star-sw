/***************************************************************************
 *
 * $Id: TMemStat.h,v 1.1 2001/06/01 02:44:01 perev Exp $
 *
 * Author: Victor Perev, Jul 2000
 ***************************************************************************
 *
 * Description:
 * Simplified version of TMemStatoryInfo class of Thomas Ullrich
 ***************************************************************************
 *  Measurement of used heap memory and total program size
 *
 *  Note: on Solaris should be linked with -lmalloc
 *
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

static  double Used();				//used heap memory in MB
static  double ProgSize();			//program size     in MB
static  void   PrintMem(const char *tit);	//print current memory
static  void   Summary();			//print total summary

private:
Double_t fLast;
Double_t fMin;
Double_t fAver;
Double_t fMax;
Double_t fRms;
Int_t    fTally;

static double fgUsed;
static TList *fgList;

ClassDef(TMemStat,0)
};

#endif
