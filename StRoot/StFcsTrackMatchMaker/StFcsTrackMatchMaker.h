// \class StFcsTrackMatchMaker
// \author Akio Ogawa
//
//   This is FCS-FowardTrack matching maker
// 

#ifndef STAR_StFcsTrackMatchMaker_HH
#define STAR_StFcsTrackMatchMaker_HH

#include "StMaker.h"

class StFwdTrackCollection;
class StFwdTrack;
class StFcsCollection;
class StFcsDb;
class StEpdGeom;
class TH1F;
class TH2F;

class StFcsTrackMatchMaker : public StMaker{
public: 
    StFcsTrackMatchMaker(const char* name="FcsTrkMatch");
    ~StFcsTrackMatchMaker();
    int Init();
    int Make();
    int Finish();

    void setFileName(char* file){mFilename=file;} 
    void setMaxDistance(float ecal, float hcal) {mMaxDistance[0]=ecal; mMaxDistance[1]=hcal;}
    void setMinEnergy(float ecal, float hcal) {mMinEnergy[0]=ecal; mMinEnergy[1]=hcal;}

private:
    StFwdTrackCollection* mFwdTrkColl=0;
    StFcsCollection* mFcsColl=0;
    StFcsDb* mFcsDb=0;
    StEpdGeom* mEpdgeo=0;

    TFile* mFile=0;
    char* mFilename=0;

    float mMaxDistance[2];
    float mMinEnergy[2];
    
    TH1F* mNtrk[4];
    TH1F* mNclu[4];
    TH1F* mCharge[3];
    TH2F* mXY[3];

    TH1F* mHdx[2];
    TH1F* mHdy[2];
    TH1F* mHdr[2];
    TH1F* mPtEt[2];
    TH2F* mPtEt2[2];

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFcsTrackMatchMaker.h,v 1.1 2021/03/30 13:34:15 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFcsTrackMatchMaker,0)
    
};

#endif
