//////////////////////////////////////////////////////////////////////////
//
//
// StL2_2008EmulatorMaker Jan Balewski  (Fall, 2007)
//
// Goal: execute all L2-algos used in 2006 
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StL2_2008EmulatorMaker
#define STAR_StL2_2008EmulatorMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StGenericL2Emulator.h"


// algos used in 2006 ...
class  L2pedAlgo;
class  L2jetAlgo;
class  L2gammaAlgo;
class  L2upsilon;


class StL2_2008EmulatorMaker : public StMaker, public StGenericL2Emulator{
private:

  L2pedAlgo   *mL2pedAlgo;
  L2jetAlgo   *mL2jetAlgo;
  L2gammaAlgo *mL2gammaEEmc;
  L2gammaAlgo *mL2gammaBEmc;
  L2upsilon   *mL2upsilon;

  void  addL2GammaAlgos2006(int runNo);
  void  addL2UpsilonAlgo2006(int runNo);
  void  addTriggerList();
  bool  getTriggerData();

public:
  StL2_2008EmulatorMaker(const char *name="L2Emul2006");
  virtual         ~StL2_2008EmulatorMaker();
  virtual Int_t   InitRun(int runumber);
  virtual Int_t   Init();
  virtual Int_t   Make();
  virtual Int_t   Finish();
  virtual void    Clear(const Option_t* = "");

  ClassDef(StL2_2008EmulatorMaker,0)
};

#endif

// $Id: StL2_2008EmulatorMaker.h,v 1.1 2007/11/13 00:12:27 balewski Exp $
//
