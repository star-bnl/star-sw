// \class StFmsDiPi0
// \author Akio Ogawa
//
//   This is FMS di-pi0 analysis
// 
//  $Id: StFmsDiPi0.h,v 1.1 2016/01/20 19:50:04 akio Exp $
//  $Log: StFmsDiPi0.h,v $
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
#include "StEnumerations.h"

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
    void setPythia(int v=1) {mPythia=v;}

private:
    StFmsDbMaker* mFmsDbMaker;
    StFmsCollection* mFmsColl;
    char* mFilename;
    TFile* mFile;

    int mPythia;
    void readPythia();
    
    enum {kNPtBin=6,kNCut=10};
    Int_t ptbin(float pt);

    TH1F* mBC;
    TH1F* mBBC;
    TH1F* mM0[kNPtBin][kNCut];
    TH1F* mM1[kNPtBin][kNPtBin][kNCut];
    TH1F* mM2[kNPtBin][kNPtBin][kNCut];
    TH1F* mZ1[kNPtBin][kNPtBin][kNCut];
    TH1F* mZ2[kNPtBin][kNPtBin][kNCut];
    TH1F* mDphi[kNPtBin][kNPtBin][kNCut];

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsDiPi0.h,v 1.1 2016/01/20 19:50:04 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsDiPi0,0);
};

#endif
