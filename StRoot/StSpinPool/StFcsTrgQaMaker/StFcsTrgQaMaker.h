/*
 *
 * \class StFcsTrgQaMaker
 *
 */

#ifndef STAR_StFcsTrgQaMaker_HH
#define STAR_StFcsTrgQaMaker_HH

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEvent/StEnumerations.h"
#include "StMaker.h"

class StFcsDbMaker;
class StFcsCollection;
class StFcsTriggerSimMaker;
class TH1F;
class TH2F;

class StFcsTrgQaMaker : public StMaker {
public: 
    StFcsTrgQaMaker( const Char_t* name = "FcsQA");
    virtual ~StFcsTrgQaMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    void setRun(int v) {mRun=v;}    
    void setFilename(char* file){mFilename=file;} 
    void setPtCh(float v) {mPtCh=v;}
    void setEcalPtThr(float v) {mEcalPtThr=v;}

protected:
    
private:
    StFcsDbMaker *mFcsDbMkr=0;
    StFcsTriggerSimMaker *mFcsTrgSimMkr=0;
    TFile *mFile=0;
    char* mFilename=0;
    int mRun=0;
    float mPtCh=0.0316;
    float mEcalPtThr=1.5;

    TH2F* mEcal[kFcsNorthSouth];
    TH2F* mHcal[kFcsNorthSouth];
    TH2F* mPres[kFcsNorthSouth];
    TH2F* mEPmap[kFcsNorthSouth];
    TH1F* mEcalNorm[kFcsNorthSouth];

    ClassDef(StFcsTrgQaMaker,1);
};

#endif

/*
 * $Id: StFcsTrgQaMaker.h,v 1.2 2020/06/01 19:34:41 akio Exp $
 * $Log: StFcsTrgQaMaker.h,v $
 * Revision 1.2  2020/06/01 19:34:41  akio
 * adding normarization histo
 *
 * Revision 1.1  2020/05/29 18:59:32  akio
 * Initial version of FCS Trigger QA maker
 *
 *
 */
