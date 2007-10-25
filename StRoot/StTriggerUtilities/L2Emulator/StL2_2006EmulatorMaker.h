//////////////////////////////////////////////////////////////////////////
//
//
// StL2_2006EmulatorMaker Jan Balewski  (Fall, 2007)
//
// Goal: execute all L2-algos used in 2006 
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StL2_2006EmulatorMaker
#define STAR_StL2_2006EmulatorMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StGenericL2Emulator.h"


// algos used in 2006 ...
class  L2pedAlgo;
class  L2jetAlgo;
class  L2gammaAlgo;


class StL2_2006EmulatorMaker : public StMaker, public StGenericL2Emulator{
private:
  //  enum {mxAH=16};
  // TH1* hA[mxAH];  
  //  void initHistoA(TString core);

  L2pedAlgo   *mL2pedAlgo;
  L2jetAlgo   *mL2jetAlgo;
  L2gammaAlgo *mL2gammaEEmc;
  L2gammaAlgo *mL2gammaBEmc;

  void  addL2GammaAlgos2006(int runNo);

  void  addTriggerList();
  bool getTriggerData();

public:
  StL2_2006EmulatorMaker(const char *name="L2Emul2006");
  virtual           ~StL2_2006EmulatorMaker();
  virtual Int_t  InitRun(int runumber);
  virtual Int_t     Init();
  virtual Int_t     Make();
  virtual Int_t     Finish();
  virtual void      Clear(const Option_t* = "");

  //  TObjArray  *mHList; // output histo access point
  // void setHList(TObjArray * x){mHList=x;}
  
  ClassDef(StL2_2006EmulatorMaker,0)
};

#endif

// $Id: StL2_2006EmulatorMaker.h,v 1.4 2007/10/25 15:30:43 balewski Exp $
//
