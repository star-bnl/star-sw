// \class StFcsEventDisplay
// \author Akio Ogawa
//
//   This is FCS trigger simulation.
// 

#ifndef STAR_StFcsTriggerSimMaker_HH
#define STAR_StFcsTriggerSimMaker_HH

#include "StMaker.h"

class StFcsDb;
class StFcsCollection;
class TFile;
class fcs_trg_base;
class link_t;
class geom_t;

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
    void setWriteQaFile(char* filename) {mQaFilename=filename;};
    void setReadPresMask(char* filename) {mPresMask=filename;};
    void setSimMode(int v, int tb=52) {mSimMode=v; mTrgTimebin=tb;}
    void setTrgTimeBin(int v) {mTrgTimebin=v;} //8 timebin = v-3 to v+4. v=52 for 49~56

    fcs_trg_base* getTriggerEmu() {return mTrgSim;}
    void runStage2(link_t ecal[], link_t hcal[], link_t pres[], geom_t& geo, link_t output[]);

private:
    StFcsDb* mFcsDb=0;
    StFcsCollection* mFcsColl=0;
    int mTrgSelect=0;
    int mDebug=0;
    char* mFilename=0;
    FILE* mFile=0;

    int mSimMode=0;      //! 0 from data, 1 for MC
    int mTrgTimebin=52;  //! center timebin for data

    fcs_trg_base* mTrgSim;

    char* mPresMask=0;
    char* mQaFilename=0;
    TFile* mQaFile=0;
    TTree* mTree=0;
    int mFlt=0;
    int mTrg=0;

    void print4B4();
    void printJP();

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFcsTriggerSimMaker.h,v 1.2 2021/05/30 21:40:56 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFcsTriggerSimMaker,0);
};

#endif
