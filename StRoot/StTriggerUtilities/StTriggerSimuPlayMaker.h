//////////////////////////////////////////////////////////////////////////
//
//
// StTriggerSimuPlayMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: exercise L0,L2 trigger simulator, lot of examples, change as needed
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StTriggerSimuPlayMaker
#define STAR_StTriggerSimuPlayMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif
class  StTriggerSimuMaker;

class StTriggerSimuPlayMaker : public StMaker {
private:
  StTriggerSimuMaker *L0trgSim;
  int mConfig;
  enum {mxAH=16};
  TH1* hA[mxAH];

  void janTest100();
  void initHistoA(TString core);

public:
  StTriggerSimuPlayMaker(const char *name="TrigSimuPlay");
  virtual           ~StTriggerSimuPlayMaker();
  void setConfig(int x) {mConfig=x;}
  
  virtual Int_t     Init();
  virtual Int_t     Make();
  virtual Int_t     Finish();
  virtual void      Clear(const Option_t* = "");
   
  TObjArray  *mHList; // output histo access point
  void setHList(TObjArray * x){mHList=x;}
  
  ClassDef(StTriggerSimuPlayMaker,0)
};

#endif

// $Id: StTriggerSimuPlayMaker.h,v 1.1 2007/10/12 20:10:24 balewski Exp $
//
