// \class StFmsDiPi0
// \author Akio Ogawa
//
//   This is FMS di-pi0 analysis
// 
//  $Id: StFmsDiPi0.h,v 1.5 2017/09/05 17:42:19 akio Exp $
//  $Log: StFmsDiPi0.h,v $
//  Revision 1.5  2017/09/05 17:42:19  akio
//  update
//
//  Revision 1.4  2016/11/17 18:57:23  akio
//  Many updates
//
//  Revision 1.3  2016/10/10 19:17:40  akio
//  *** empty log message ***
//
//  Revision 1.2  2016/06/08 16:28:09  akio
//  *** empty log message ***
//
//  Revision 1.1  2016/01/20 19:58:55  akio
//  *** empty log message ***
//
//  Revision 1.1  2016/01/20 19:50:04  akio
//  *** empty log message ***
//
//  Revision 1.1  2015/10/20 19:55:51  akio
//  Initial version of FMS event display
//
//

#ifndef STAR_StFmsDiPi0_HH
#define STAR_StFmsDiPi0_HH

#include "StMaker.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TTree.h"
#include "TChain.h"

class StFmsDbMaker;
class StFmsCollection;

class StFmsDiPi0 : public StMaker{
public: 
    StFmsDiPi0(const Char_t* name="FmsDiPi0");
    ~StFmsDiPi0();
    Int_t Init();
    Int_t Make();
    Int_t Finish();

    void setFileName(char* file){mFilename=file;} 
    void setTreeFileName(char* file){mTreeFilename=file;} 
    void setWriteTree(int v=1) {mWriteTree=v;}
    void setReadTree(TChain* c) {mReadTree=1; mChain=c;}
    void setBBCCut(float v1, float v2, float v3, float v4){mBBCCut1=v1;  mBBCCut2=v2;  mBBCCut3=v3;  mBBCCut4=v4;}

    void setPythia(int v=1) {mPythia=v;}

private:
    StFmsDbMaker* mFmsDbMaker=0;
    StFmsCollection* mFmsColl=0;
    char* mFilename=0;
    TFile* mFile=0;

    int mPythia=0;
    void readPythia();
    
    enum {kNPtBin=6,kNCut=20};
    Int_t ptbin(float pt);

    Int_t mBBCCut1=0;
    Int_t mBBCCut2=0;
    Int_t mBBCCut3=0;
    Int_t mBBCCut4=0;

    TH1F* mBC=0;
    TH1F* mBBC=0;
    TH1F* mBBCAG=0;
    TH1F* mBBCM=0;
    TH1F* mBBCMAG=0;
    TH1F* mTOF=0;
    TH1F* mTOFAG=0;
    TH2F* mBBCTOF=0;
    TH2F* mBBCMTOF=0;
    TH2F* mBBCBBCM=0;
    TH2F* mTOFTOF=0;
    
    TH2F* mMass0=0;
    TH2F* mMass1=0;
    TH2F* mMass2=0;
    TH2F* mEne=0;
    TH2F* mPt=0;

    TH1F* mM0[kNPtBin][kNCut+1];
    TH1F* mPhi0[kNPtBin][kNCut+1];
    TH2F* mEtaPhi0[kNPtBin][kNCut+1];

    TH1F* mM1[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mM2[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mZ1[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mZ2[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mE1[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mE2[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mPt1[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mPt2[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mEta1[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mEta2[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mPhi1[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mPhi2[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mDphi[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mBbce[kNPtBin][kNPtBin][kNCut+1];
    TH1F* mTofm[kNPtBin][kNPtBin][kNCut+1];
    TH2F* mPhi1Dphi[kNPtBin][kNPtBin][kNCut+1];

    char* mTreeFilename=0;
    int mWriteTree=0;
    int mReadTree=0;
    TFile* mTreeFile=0;
    TTree* mTree=0;
    TTree* mChain=0;

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsDiPi0.h,v 1.5 2017/09/05 17:42:19 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsDiPi0,0);
};

#endif
