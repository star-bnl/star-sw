// \class StFcsEventDisplay
// \author Akio Ogawa
//
//   This is FCS trigger simulation.
// 

#ifndef STAR_StFcsTriggerSimMaker_HH
#define STAR_StFcsTriggerSimMaker_HH

#include "StMaker.h"
#include <stdint.h>

class StFcsDb;
class StFcsCollection;
class StMuFcsCollection;
class TFile;
class fcs_trg_base;
class link_t;
class geom_t;
class TTree;
class TH1F;
class StFcsHit;
class StMuFcsHit;

class StFcsTriggerSimMaker : public StMaker{
public: 
    StFcsTriggerSimMaker(const char* name="FcsTrgSim");
    ~StFcsTriggerSimMaker();
    int Init();
    int InitRun(int runNumber);
    int Make();
    int Finish();    
    
    void setTrigger(int v) {mTrgSelect=v;};
    void setDebug(int v) {mDebug=v;};
    void setWriteEventText(char* filename) {mFilename=filename;};
    void setWriteQaTree(char* filename) {mQaTreeFilename=filename;};
    void setWriteQaHist(char* filename) {mQaHistFilename=filename;};
    void setReadPresMask(char* filename) {mPresMask=filename;};
    void setSimMode(int v, int tb=52) {mSimMode=v; mTrgTimebin=tb;}
    void setTrgTimeBin(int v) {mTrgTimebin=v;} //8 timebin = v-3 to v+4. v=52 for 49~56
    void setThresholdFile(char* file) {mThresholdFile=file;}
    void setThresholdDb(int run) {mThresholdDb=run;}
        
    //factor= 1(ET Match), 0(E Match), 0.5(halfway)  
    void setEtGain(float v) {mEtFactor=v;} 

    fcs_trg_base* getTriggerEmu() {return mTrgSim;}
    
    template<typename T>  void feedADC(T* hit, int ns, int ehp, uint16_t data_array[]);

    //this is for just running stage2 from macro
    void runStage2(link_t ecal[], link_t hcal[], link_t pres[], geom_t &geo, link_t output[]);

private:
    StFcsDb* mFcsDb=0;
    StFcsCollection* mFcsColl=0;
    StMuFcsCollection* mMuFcsColl = 0;

    int mTrgSelect=0;
    int mDebug=0;
    char* mFilename=0;
    FILE* mFile=0;

    int mSimMode=0;      //! 0 from data, 1 for MC
    int mTrgTimebin=52;  //! center timebin for data

    fcs_trg_base* mTrgSim;

    char* mPresMask=0;
    char* mQaTreeFilename=0;
    char* mQaHistFilename=0;
    TFile* mQaTreeFile=0;
    TFile* mQaHistFile=0;
    TTree* mTree=0;
    TH1F* mTrgRate=0;
    int mFlt=0;
    int mTcu=0;
    int mTrg=0;
    float mEtFactor=1.0; 
    char* mThresholdFile=0;
    int mThresholdDb=0;

    void print4B4();
    void printJP();
    void readThresholdFile();
    void readThresholdDb();

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFcsTriggerSimMaker.h,v 1.2 2021/05/30 21:40:56 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFcsTriggerSimMaker,0);
};

#endif
