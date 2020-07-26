// \class StFcsEventDisplay
// \author Akio Ogawa
//
//   This is FCS trigger simulation.
// 

#ifndef STAR_StFcsTriggerSimMaker_HH
#define STAR_StFcsTriggerSimMaker_HH

#include "StMaker.h"
#include "StRoot/RTS/src/DAQ_FCS/fcs_data_c.h"
#include "StRoot/RTS/src/TRG_FCS/fcs_trg_base.h"

class StFcsDbMaker;
class StFcsCollection;
class TFile;

class StFcsTriggerSimMaker : public StMaker{
public: 
    StFcsTriggerSimMaker(const Char_t* name="FcsTrgSim");
    ~StFcsTriggerSimMaker();
    Int_t Init();
    Int_t InitRun(Int_t runNumber);
    Int_t Make();
    Int_t Finish();    
    
    void setTrigger(int v) {mTrgSelect=v;};
    void setDebug(int v) {mDebug=v;};
    void setWriteEventText(char* filename) {mFilename=filename;};
    void setWriteQaFile(char* filename) {mQaFilename=filename;};
    void setReadPresMask(char* filename) {mPresMask=filename;};

    fcs_trg_base* getTriggerEmu() {return mTrgSim;}

private:
    StFcsDbMaker* mFcsDbMaker=0;
    StFcsCollection* mFcsColl=0;
    Int_t mTrgSelect=0;
    Int_t mDebug=0;
    char* mFilename=0;
    FILE* mFile=0;

    fcs_data_c* mFcsDataC;
    fcs_trg_base* mTrgSim;

    char* mPresMask=0;
    char* mQaFilename=0;
    TFile* mQaFile=0;
    TTree* mTree=0;
    Int_t mFlt=0;
    Int_t mTrg=0;

    void print4B4();
    void printJP();

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFcsTriggerSimMaker.h,v 1.3 2020/07/24 17:22:39 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFcsTriggerSimMaker,0);
};

#endif
