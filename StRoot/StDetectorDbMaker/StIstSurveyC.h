#ifndef StIstSurvey_h
#define StIstSurvey_h

#include "St_SurveyC.h"

class StidsOnTpc : public St_SurveyC {
 public:
  static StidsOnTpc* 	instance();
  StidsOnTpc(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StidsOnTpc() {fgInstance = 0;}
 private:
  static StidsOnTpc* fgInstance;
  ClassDef(StidsOnTpc,1) //C++ TChair for idsOnTpc
};


class StIstpstOnIds : public St_SurveyC {
 public:
  static StIstpstOnIds* 	instance();
  StIstpstOnIds(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StIstpstOnIds() {fgInstance = 0;}
 private:
  static StIstpstOnIds* fgInstance;
  ClassDef(StIstpstOnIds,1) //C++ TChair for pstOnIds
};

class StistOnPst : public St_SurveyC {
 public:
  static StistOnPst* 	instance();
  StistOnPst(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StistOnPst() {fgInstance = 0;}
 private:
  static StistOnPst* fgInstance;
  ClassDef(StistOnPst,1) //C++ TChair for istOnPst
};
class StLadderOnIst : public St_SurveyC {
 public:
  static StLadderOnIst* 	instance();
  StLadderOnIst(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StLadderOnIst() {fgInstance = 0;}
 private:
  static StLadderOnIst* fgInstance;
  ClassDef(StLadderOnIst,1) //C++ TChair for LadderOnIst
};
class StistSensorOnLadder : public St_SurveyC {
 public:
  static StistSensorOnLadder* 	instance();
  StistSensorOnLadder(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StistSensorOnLadder() {fgInstance = 0;}
 private:
  static StistSensorOnLadder* fgInstance;
  ClassDef(StistSensorOnLadder,1) //C++ TChair for istSensorOnLadder
};

#endif
