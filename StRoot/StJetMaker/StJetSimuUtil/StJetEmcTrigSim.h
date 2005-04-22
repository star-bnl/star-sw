//StJetEmcTrigSim.h
//M.L. Miller (MIT)
//3/05

//Simple wrapper around Alex Stolpovski's global function

#ifndef StJetEmcTrigSim_HH
#define StJetEmcTrigSim_HH

#include "StMaker.h"

class St_db_Maker;
class StBemcTables;
class StMuDstMaker;

class StJetEmcTrigSim : public StMaker
{
public:

    enum AlexReturnCode {kProblem = 56}; // some integer number to be used as return code (not bool)

    StJetEmcTrigSim(const char* maker_name, St_db_Maker* db, StMuDstMaker* m);
    virtual ~StJetEmcTrigSim();
    
    virtual Int_t Init();
    virtual Int_t InitRun(Int_t runId);
    virtual Int_t Make();
    virtual Int_t Finish();

    //accessors:
    int is2004HighTowerTriggeredEvent() const {return mIs2004Ht;}    
    int hiTowerAdc12Bit() const {return mHiTowerAdc12bit;}
    int hiTowerAdc6Bit() const {return mHiTowerAdc6bit;}
    UInt_t hiTowerTrigAdc() const {return mHiTowerTrigAdc;}
    Float_t hiTowerEt() const {return mHiTowerEt;}
    int hiTowerId() const {return mHiTowerId;}
    
    void setPrint(bool v) {mPrint=v;}

private:

    int is2004HighTowerTriggeredEvent(StEmcCollection* emccol, int & hiTowerAdc6bit);
    Int_t getHiTower(StEmcCollection* emccol, Float_t& hiTowerEt, UInt_t& hiTowerTrigADC, int& hiTowerAdc12bit);

    
    St_db_Maker* mDbMaker;//!
    StMuDstMaker* mMuDstMaker;//!
    bool mPrint; 
    StBemcTables* mTables; //!

    int mHiTowerAdc12bit;
    int mHiTowerAdc6bit;
    UInt_t mHiTowerTrigAdc;
    Float_t mHiTowerEt;
    int mHiTowerId;
    int mIs2004Ht;
    
    ClassDef(StJetEmcTrigSim,0)
};

#endif
