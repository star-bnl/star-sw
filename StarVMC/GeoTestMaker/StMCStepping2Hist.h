// $Id: StMCStepping2Hist.h,v 1.3 2013/04/20 21:54:07 perev Exp $
//
//
// Class StMCStepping2Hist
// ------------------


#ifndef STMC_STEPPING2HIST_H
#define STMC_STEPPING2HIST_H

#include "TString.h"
#include "TArrayF.h"
#include "TLorentzVector.h"
#include "StMCStepping.h"

class StTGeoProxy;
class StTGeoHitShape;
class My2Hist;
class StiELossTrk;

class StMCStepping2Hist : public StMCStepping
{
public:
         StMCStepping2Hist(const char *name="",const char *tit="");
virtual ~StMCStepping2Hist();   
    // methods
virtual int  Fun();
virtual void Print (const Option_t* opt=0) const;
virtual void Finish(const Option_t* opt=0);
static StMCStepping2Hist *Instance() {return fThis;}
private:
static const char *Alias(const char *modu);
void FillHist(int flag);
protected:
char fFist[1];
double fSensMaxR;
double fSensMaxZ;
double fTotRadL;
double fTotOrt2;
double fVolRadL;
   int fVolHits;

      StiELossTrk 	*fELossTrk[2];
      My2Hist 		*fMy2Hist;
const StTGeoHitShape 	*fHitShape ;
char fLast[1];
TString fModName;

private:
void FillHist();

static StMCStepping2Hist *fThis;

ClassDef(StMCStepping2Hist,0) // 
};
#endif //STMC_STEPPING2HIST_H   
   
