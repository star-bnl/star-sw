// \class StFmsOfflineQaMaker
// \author Akio Ogawa
//
//   This is for FMS offline QA
// 
//  $Id: StFmsOfflineQaMaker.h,v 1.2 2016/06/08 19:55:11 akio Exp $
//  $Log: StFmsOfflineQaMaker.h,v $
//  Revision 1.2  2016/06/08 19:55:11  akio
//  applying coverity report
//
//  Revision 1.1  2016/01/26 19:54:33  akio
//  Separated from StFmsFpsMaker... This is for FMS offline QA and also FMS-FPS alignments
//

#ifndef STAR_StFmsOfflineQaMaker_HH
#define STAR_StFmsOfflineQaMaker_HH

#include "StMaker.h"
#include "StEnumerations.h"

class StFmsDbMaker;
class StFmsCollection;
class TH2F;

class StFmsOfflineQaMaker : public StMaker{
public: 
    StFmsOfflineQaMaker(const Char_t* name="FmsOfflineQaMaker");
    ~StFmsOfflineQaMaker();
    Int_t Init();
    Int_t Make();
    Int_t Finish();
    
    void setFileName(char* file){mFilename=file;}

    void setPrint(int v=1){mPrint=v;}
    void print();
    
private:
    StFmsDbMaker* mFmsDbMaker=0;
    StFmsCollection* mFmsColl=0;
    int mBunch=0;
    int mTrigger=0;
    
    char* mFilename=0;
    TFile* mFile=0;
    int mPrint=0;
    
    //QA & alignment related
    enum {NCUT1=10};   
    enum {NCUT2=11};  
    
    //total energy ratio
    TH1F* mERatio[2];
    TH1F* mBC=0;
    TH1F* mTrig[2];

    //hit related
    TH1F* mFmsAdc=0;
    TH2F* mFmsHitLarge[2];
    TH2F* mFmsHitSmall[2];
    TH1F* mFpsMip[3];
    
    //cluster related
    TH1F* mNTow[2];
    TH2F* mNTowE[2];
    TH1F* mSigmax[2];
    TH1F* mSigmin[2];
    TH2F* mSigmaxE[2];
    TH1F* mChi2[2];
    TH2F* mCluXY[2][2];
    TH1F* mCluEta[6];

    //point related
    TH2F* mH2[kFpsNQuad][kFpsNLayer][NCUT1];
    TH1F* mHd[kFpsNQuad][kFpsNLayer][NCUT1];
    TH2F* mHd2[kFpsNQuad][kFpsNLayer][NCUT1];

    TH1F* mHn[NCUT1];
    TH1F* mHene[NCUT1];
    TH1F* mHelo[NCUT1];
    TH1F* mHpt[NCUT1];
    TH2F* mHept[NCUT1];
    TH1F* mHeta[NCUT1];
    TH1F* mHphi[NCUT1];
    TH1F* mHx[NCUT1];
    TH1F* mHy[NCUT1];
    TH1F* mHdxL[NCUT1];
    TH1F* mHdxS[NCUT1];
    TH2F* mHxy[NCUT1];
    TH1F* mHpid[NCUT1];
    TH1F* mHpid2[NCUT1];    
    
    TH1F* mPn[NCUT2];
    TH1F* mPene[NCUT2];
    TH1F* mPpt[NCUT2];
    TH2F* mPept[NCUT2];
    TH1F* mPeta[NCUT2];
    TH1F* mPphi[NCUT2];
    TH2F* mPpid[NCUT2];
    TH1F* mPm1[NCUT2];
    TH1F* mPm2[NCUT2];
    TH1F* mPzgg[NCUT2];
    TH1F* mPdgg[NCUT2];
    TH1F* mPr30[NCUT2];
    TH1F* mPr100[NCUT2];
    TH2F* mPxy[NCUT2];

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsOfflineQaMaker.h,v 1.2 2016/06/08 19:55:11 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsOfflineQaMaker,0);
};

#endif
