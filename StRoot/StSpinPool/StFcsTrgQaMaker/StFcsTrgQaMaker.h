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
    void setS2off(int v) {mS2off=v;}

protected:
    
private:
    StFcsDb *mFcsDb=0;
    StFcsTriggerSimMaker *mFcsTrgSimMkr=0;
    TFile *mFile=0;
    char* mFilename=0;
    int mRun=0;
    float mPtCh=0.0316;
    float mEcalPtThr=1.5;

    //marker.adc_start = 7
    //marker.s1_out_start = marker.adc_start + 11 = 18
    //marker.s2_in_start = marker.s1_out_start + 2  = 20
    //marker.s2_to_s3_start = marker.s2_in_start + 15 = 35
    //marker.s3_in_start = marker.s2_to_s3_start + 8 = 43
    //marker.dsm_out_start=marker.s3_in_start + 14 = 57
    //So... DEPIO DSM out is 50 tb later than ADC
    //center trig tb=50 => tb=47~54 and 47=marker.adc_start(7)+5xing*8tb/xing
    //Thus.... mS3off=47+50 = 97
    int mS3off=97; 
    int mS2off=97; //Tonko says same as S3off

    TH2F* mTow[2];

    TH1F* mETot[2];
    TH1F* mHTot[2];

    TH1F* mEHT[2];
    TH2F* mEHTMap;
    TH1F* mHHT[2];
    TH2F* mHHTMap;

    TH1F* mJP[2][5];
    TH1F* mJPMap[3];

    TH1F* mE4x4;
    TH1F* mEM4x4;
    TH1F* mELE4x4;
    TH1F* mERatio;
    TH1F* mELERatio;

    TH1F* mH4x4;
    TH1F* mEH4x4;
    TH1F* mHAD4x4;
    TH1F* mHRatio;
    TH1F* mHADRatio;

    TH2F* mE4x4Map[4];
    TH2F* mELE4x4Map[2];
    TH2F* mEH4x4Map[3];

    TH1F* mDsmOut;
    TH1F* mDepOut;
    TH1F* mTcuBit;
    TH1F* mTcuDep;
    TH1F* mSimDep;
    
    TH2F* mAdc[3];

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
