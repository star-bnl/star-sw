#ifndef StGmtSurvey_h
#define StGmtSurvey_h

#include "St_SurveyC.h"

class StGmtOnTpc : public St_SurveyC {
 public:
  static StGmtOnTpc* 	instance();
  StGmtOnTpc(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StGmtOnTpc() {fgInstance = 0;}
 private:
  static StGmtOnTpc* fgInstance;
  ClassDef(StGmtOnTpc,1) //C++ TChair
};


class StGmtOnModule : public St_SurveyC {
 public:
  static StGmtOnModule* 	instance();
  StGmtOnModule(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StGmtOnModule() {fgInstance = 0;}
 private:
  static StGmtOnModule* fgInstance;
  ClassDef(StGmtOnModule,1) //C++ TChair
};

#endif
