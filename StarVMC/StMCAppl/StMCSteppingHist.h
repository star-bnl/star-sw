// $Id: StMCSteppingHist.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCSteppingHist
// ------------------


#ifndef STMC_STEPPINGHIST_H
#define STMC_STEPPINGHIST_H

#include "TString.h"
#include "TArrayF.h"
#include "TLorentzVector.h"
#include "StMCStepping.h"
class TObjArray;
class StMCSteppingHist : public StMCStepping
{
public:
         StMCSteppingHist(const char *name="",const char *tit="");
virtual ~StMCSteppingHist();   
    // methods
virtual int  Fun();
virtual void Print (const Option_t* opt=0) const;
virtual void Finish(const Option_t* opt=0);
static StMCSteppingHist *Instance() {return fThis;}
void TestTGeo();
protected:
char fFist;
TObjArray *fVols;
TObjArray *fMats;
double fPrevEps;
char fLast;
private:
void Fill(TObjArray *arr);
void Sort(TObjArray *arr);
void SummArr();
void FillVolMat();

static StMCSteppingHist *fThis;

ClassDef(StMCSteppingHist,0) // 
};

#endif //STMC_STEPPINGHIST_H   
   

