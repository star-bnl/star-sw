//////////////////////////////////////////////////////////////////////////
//
//
// StTriggerSimuMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: generate trigger response based on ADC
// implemented BEMC,EEMC,....
// >StTriggerSimu/*SUB*/St*SUB*TriggerSimu.h
// >where *SUB* are the subsystems: Eemc, Bemc, Bbc, L2,.... 
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StTriggerSimuMaker
#define STAR_StTriggerSimuMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StEemcTriggerSimu;
class St_db_Maker;

class StTriggerSimuMaker : public StMaker {
private:
  int mYear;
  int mMCflag; // set yo 0 for real data
  St_db_Maker *mDbMk;
  void addTriggerList();

public:
    StTriggerSimuMaker(const char *name="EMCtrigger");
    virtual           ~StTriggerSimuMaker();
    void    useEemc();
    void    setMC(int x) {mMCflag=x;}
    //hang all activated trigger detectors below
    StEemcTriggerSimu *eemc;

    TObjArray  *mHList; // output histo access point
    void setHList(TObjArray * x){mHList=x;}
    vector <int> mTriggerList;
    virtual Int_t     Init();
    virtual Int_t     Make();
    virtual Int_t     Finish();
    virtual void      Clear(const Option_t* = "");
    virtual Int_t InitRun  (int runumber);
    bool    isTrigger(int trigId);   
    void    setDbMaker(St_db_Maker *dbMk) { mDbMk=dbMk;}
 
    ClassDef(StTriggerSimuMaker,0)
};
   
#endif



// $Id: StTriggerSimuMaker.h,v 1.2 2007/07/21 23:35:24 balewski Exp $
//
// $Log: StTriggerSimuMaker.h,v $
// Revision 1.2  2007/07/21 23:35:24  balewski
// works for M-C
//
// Revision 1.1  2007/07/20 21:01:41  balewski
// start
//
