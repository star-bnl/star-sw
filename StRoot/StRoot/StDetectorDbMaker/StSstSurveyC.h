#ifndef StSstSurvey_h
#define StSstSurvey_h

#include "St_SurveyC.h"

class StoscOnTpc : public St_SurveyC {
 public:
  static StoscOnTpc* 	      instance();
  StoscOnTpc(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StoscOnTpc() {fgInstance = 0;}
 private:
  static StoscOnTpc* fgInstance;
  ClassDef(StoscOnTpc,1) //C++ TChair for oscOnTpc
};

class StsstOnOsc : public St_SurveyC {
 public:
  static StsstOnOsc* 	      instance();
  StsstOnOsc(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StsstOnOsc() {fgInstance = 0;}
 private:
  static StsstOnOsc* fgInstance;
  ClassDef(StsstOnOsc,1) //C++ TChair for sstOnOsc
};

class StsstLadderOnSst : public St_SurveyC {
 public:
  static StsstLadderOnSst*    instance();
  StsstLadderOnSst(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StsstLadderOnSst() {fgInstance = 0;}
 private:
  static StsstLadderOnSst* fgInstance;
  ClassDef(StsstLadderOnSst,1) //C++ TChair for sstLadderOnSst
};
class StsstSensorOnLadder : public St_SurveyC {
 public:
  static StsstSensorOnLadder* instance();
  StsstSensorOnLadder(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StsstSensorOnLadder() {fgInstance = 0;}
 private:
  static StsstSensorOnLadder* fgInstance;
  ClassDef(StsstSensorOnLadder,1) //C++ TChair for sstSensorOnLadder
};

#endif
