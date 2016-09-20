#ifndef StSstSurvey_h
#define StSstSurvey_h

#include "St_SurveyC.h"

class StSstOnGlobal : public St_SurveyC {
 public:
  static StSstOnGlobal* 	instance();
  StSstOnGlobal(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSstOnGlobal() {fgInstance = 0;}
 private:
  static StSstOnGlobal* fgInstance;
  ClassDef(StSstOnGlobal,1) //C++ TChair for SstOnGlobal
};

class StSstSectorsOnGlobal : public St_SurveyC {
 public:
  static StSstSectorsOnGlobal* 	instance();
  StSstSectorsOnGlobal(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSstSectorsOnGlobal() {fgInstance = 0;}
 private:
  static StSstSectorsOnGlobal* fgInstance;
  ClassDef(StSstSectorsOnGlobal,1) //C++ TChair for SstSectorsOnGlobal
};

class StSstLaddersOnSectors : public St_SurveyC {
 public:
  static StSstLaddersOnSectors* 	instance();
  StSstLaddersOnSectors(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSstLaddersOnSectors() {fgInstance = 0;}
 private:
  static StSstLaddersOnSectors* fgInstance;
  ClassDef(StSstLaddersOnSectors,1) //C++ TChair for SstLaddersOnSectors
};
class StSstWafersOnLadders : public St_SurveyC {
 public:
  static StSstWafersOnLadders* 	instance();
  StSstWafersOnLadders(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSstWafersOnLadders() {fgInstance = 0;}
 private:
  static StSstWafersOnLadders* fgInstance;
  ClassDef(StSstWafersOnLadders,1) //C++ TChair for SstWafersOnLadders
};

#endif
