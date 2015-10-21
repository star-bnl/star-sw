// \class StFmsFpsMaker
// \author Akio Ogawa
//
//   This is analysis for FMS-FPS correlations.
// 
//  $Id: StFmsFpsMaker.h,v 1.3 2015/10/21 15:51:01 akio Exp $
//  $Log: StFmsFpsMaker.h,v $
//  Revision 1.3  2015/10/21 15:51:01  akio
//  Add more QA for debugging FMS reconstruciton code
//
//  Revision 1.2  2015/09/18 18:50:06  akio
//  cleaning up and adding QA histos
//
//  Revision 1.1  2015/09/02 14:56:12  akio
//  Initial version of FMS-FPS correation analysis
//

#ifndef STAR_StFmsFpsMaker_HH
#define STAR_StFmsFpsMaker_HH

#include "StMaker.h"
#include "StEnumerations.h"

class StFmsDbMaker;
class StFmsCollection;
class TH2F;

class StFmsFpsMaker : public StMaker{
public: 
    StFmsFpsMaker(const Char_t* name="FmsFps");
    ~StFmsFpsMaker();
    Int_t Init();
    Int_t Make();
    Int_t Finish();
    
    //Read MuDST hits if available, and update FPS hits in StEvent using current DB values
    //if this is set to 0 (default), use hits as is (new DB values will NOT be applied)
    void setReadMuDST(int v=1){mReadMuDST=v;}         

    //max distance to associate FPS slat to FMS point
    void setMaxDistanceToAssociate(float v) {mMaxDistanceToAssociate=v;}

    //0=take closest slat, 1=take sum of all slat associated
    void setPidMethod(int v) {mPidMethod=v;}
    
    void setQA(char* file){mQA=true; mFilename=file;} //enable alighment histograms for QA
    void setPrint(int v=1){mPrint=v;}
    void print();
    
private:
    StFmsDbMaker* mFmsDbMaker;
    StFmsCollection* mFmsColl;
    
    bool mQA;
    char* mFilename;
    TFile* mFile;
    int mPrint;
    
    int mReadMuDST; 
    void readMuDST();
    
    int mMaxDistanceToAssociate;
    int mPidMethod;
	
    void corrFmsFps();
    void pid(int opt=0);
    void isolationCone();
    
    //QA & alignment related
    enum {NCUT1=9};  //0:all, 1:E>3GeV,     2:FV, 3:LSgap, 4:NSgap 5:PID=gamma, 6:PID=hadron, 7:PID=electron, 8:others
    enum {NCUT2=9};  //0:all, 1:E1,E2>5GeV, 2:FV, 3:LSgap, 4:NSGap 5:PID=gg, 6:PID=hh, 7:ee, 8:other PID combos
    
    void fmsFpsAlignment(); 
    
    //total energy ratio
    TH1F* mERatio[2];

    //hit related
    TH1F* mFmsAdc;
    TH2F* mFmsHitLarge;
    TH2F* mFmsHitSmall;
    TH1F* mFpsMip[3];
    
    //cluster related
    TH1F* mNTow[2];
    TH2F* mNTowE[2];
    TH1F* mSigmax[2];
    TH1F* mSigmin[2];
    TH2F* mSigmaxE[2];
    TH1F* mChi2[2];
    TH2F* mCluXY[2];

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
    TH2F* mHxy[NCUT1];
    TH1F* mHpid[NCUT1];
    TH1F* mHpid2[NCUT1];    
    
    TH1F* mPn[NCUT1];
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
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsFpsMaker.h,v 1.3 2015/10/21 15:51:01 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsFpsMaker,0);
};

#endif
