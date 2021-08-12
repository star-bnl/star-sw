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

class StFcsDb;
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
    void setS3off(int v) {mS3off=v;}

protected:
    
private:
    StFcsDb *mFcsDb=0;
    StFcsTriggerSimMaker *mFcsTrgSimMkr=0;
    TFile *mFile=0;
    char* mFilename=0;
    int mRun=0;
    float mPtCh=0.0316;
    float mEcalPtThr=1.5;
    int mS3off=96;

    TH1F* mETot[4];
    TH1F* mHTot[4];
    TH1F* mEHT[4];
    TH1F* mHHT[4];
    TH1F* mJP[2][4];

    TH1F* mE4b4[4];
    TH1F* mH4b4[4];
    TH1F* mPOR[4];

    TH1F* mSum[5][4];
    TH1F* mEHR[5][4];
    TH2F* mSumTot[2];

    TH2F* mDEm[4];
    TH2F* mDHad[4];
    TH2F* mDGam[4];
    TH2F* mDEle[4];
    TH2F* mDJP[4];

    TH2F* mEcal[kFcsNorthSouth];
    TH2F* mHcal[kFcsNorthSouth];
    TH2F* mPres[kFcsNorthSouth];
    TH2F* mEPmap[kFcsNorthSouth];
    TH1F* mEcalNorm[kFcsNorthSouth];

    TH1F* mDsmOut;
    TH1F* mDepOut;
    TH1F* mTcuBit;
    TH1F* mTcuDep;
    TH1F* mSimDep;
    
    ClassDef(StFcsTrgQaMaker,1);
};

#endif

/*
 * $Id: StFcsTrgQaMaker.h,v 1.5 2021/05/30 21:41:59 akio Exp $
 * $Log: StFcsTrgQaMaker.h,v $
 * Revision 1.5  2021/05/30 21:41:59  akio
 * A lots of update for trigger comissionong Run21 OO200
 *
 * Revision 1.4  2021/03/30 13:31:27  akio
 * StFcsDbMAker->StFcsDB
 *
 * Revision 1.3  2020/07/24 17:21:31  akio
 * adding hist for DsmOut
 *
 * Revision 1.2  2020/06/01 19:34:41  akio
 * adding normarization histo
 *
 * Revision 1.1  2020/05/29 18:59:32  akio
 * Initial version of FCS Trigger QA maker
 *
 *
 */
