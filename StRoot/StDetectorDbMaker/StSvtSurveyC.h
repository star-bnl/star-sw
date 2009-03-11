#ifndef StSvtSurvey_h
#define StSvtSurvey_h

#include "St_SurveyC.h"

class StSvtOnGlobal : public St_SurveyC {
 public:
  static StSvtOnGlobal* 	instance();
  StSvtOnGlobal(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSvtOnGlobal() {fgInstance = 0;}
 private:
  static StSvtOnGlobal* fgInstance;
  ClassDef(StSvtOnGlobal,1) //C++ TChair for SvtOnGlobal
};


class StSvtShellOnGlobal : public St_SurveyC {
 public:
  static StSvtShellOnGlobal* 	instance();
  StSvtShellOnGlobal(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSvtShellOnGlobal() {fgInstance = 0;}
 private:
  static StSvtShellOnGlobal* fgInstance;
  ClassDef(StSvtShellOnGlobal,1) //C++ TChair for SvtShellOnGlobal
};

class StSvtLadderOnSurvey : public St_SurveyC {
 public:
  static StSvtLadderOnSurvey* 	instance();
  StSvtLadderOnSurvey(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSvtLadderOnSurvey() {fgInstance = 0;}
 private:
  static StSvtLadderOnSurvey* fgInstance;
  ClassDef(StSvtLadderOnSurvey,1) //C++ TChair for SvtLadderOnSurvey
};
class StSvtLadderOnShell : public St_SurveyC {
 public:
  static StSvtLadderOnShell* 	instance();
  StSvtLadderOnShell(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSvtLadderOnShell() {fgInstance = 0;}
 private:
  static StSvtLadderOnShell* fgInstance;
  ClassDef(StSvtLadderOnShell,1) //C++ TChair for SvtLadderOnShell
};
class StSvtWaferOnLadder : public St_SurveyC {
 public:
  static StSvtWaferOnLadder* 	instance();
  StSvtWaferOnLadder(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSvtWaferOnLadder() {fgInstance = 0;}
 private:
  static StSvtWaferOnLadder* fgInstance;
  ClassDef(StSvtWaferOnLadder,1) //C++ TChair for SvtWaferOnLadder
};

#endif
