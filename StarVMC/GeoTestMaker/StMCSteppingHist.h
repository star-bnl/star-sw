// $Id: StMCSteppingHist.h,v 1.1 2009/03/25 23:15:11 perev Exp $
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
class TProfile2D;
class TCanvas;
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
TProfile2D    *mH[20];
TCanvas *mC[20];
int  rStep;
double mRadL;
char fLast;
private:
void Fill(TObjArray *arr);
void Sort(TObjArray *arr);
void SummArr();
void FillVolMat();
void FillHist();

static StMCSteppingHist *fThis;

ClassDef(StMCSteppingHist,0) // 
};

#endif //STMC_STEPPINGHIST_H   
   
