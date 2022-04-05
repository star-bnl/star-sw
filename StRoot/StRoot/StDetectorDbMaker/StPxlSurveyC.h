#ifndef StPxlSurvey_h
#define StPxlSurvey_h

#include "St_SurveyC.h"

class StPxlpstOnIds : public St_SurveyC {
 public:
  static StPxlpstOnIds* 	instance();
  StPxlpstOnIds(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StPxlpstOnIds() {fgInstance = 0;}
 private:
  static StPxlpstOnIds* fgInstance;
  ClassDef(StPxlpstOnIds,1) //C++ TChair for pstOnIds
};

class StpxlOnPst : public St_SurveyC {
 public:
  static StpxlOnPst*     	instance();
  StpxlOnPst(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StpxlOnPst() {fgInstance = 0;}
 private:
  static StpxlOnPst* fgInstance;
  ClassDef(StpxlOnPst,1) //C++ TChair for pxlOnPst
};
class StpxlHalfOnPxl : public St_SurveyC {
 public:
  static StpxlHalfOnPxl* 	instance();
  StpxlHalfOnPxl(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StpxlHalfOnPxl() {fgInstance = 0;}
 private:
  static StpxlHalfOnPxl* fgInstance;
  ClassDef(StpxlHalfOnPxl,1) //C++ TChair for pxlHalfOnPxl
};
class StpxlSectorOnHalf : public St_SurveyC {
 public:
  static StpxlSectorOnHalf* 	instance();
  StpxlSectorOnHalf(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StpxlSectorOnHalf() {fgInstance = 0;}
 private:
  static StpxlSectorOnHalf* fgInstance;
  ClassDef(StpxlSectorOnHalf,1) //C++ TChair for pxlSectorOnHalf
};
class StpxlLadderOnSector : public St_SurveyC {
 public:
  static StpxlLadderOnSector* 	instance();
  StpxlLadderOnSector(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StpxlLadderOnSector() {fgInstance = 0;}
 private:
  static StpxlLadderOnSector* fgInstance;
  ClassDef(StpxlLadderOnSector,1) //C++ TChair for pxlLadderOnSector
};
class StpxlSensorOnLadder : public St_SurveyC {
 public:
  static StpxlSensorOnLadder* 	instance();
  StpxlSensorOnLadder(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StpxlSensorOnLadder() {fgInstance = 0;}
 private:
  static StpxlSensorOnLadder* fgInstance;
  ClassDef(StpxlSensorOnLadder,1) //C++ TChair for pxlSensorOnLadder
};

#endif
