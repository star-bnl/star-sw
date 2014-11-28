#ifndef StSsdSurvey_h
#define StSsdSurvey_h

#include "St_SurveyC.h"

class StSsdOnGlobal : public St_SurveyC {
 public:
  static StSsdOnGlobal* 	instance();
  StSsdOnGlobal(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSsdOnGlobal() {fgInstance = 0;}
 private:
  static StSsdOnGlobal* fgInstance;
  ClassDef(StSsdOnGlobal,1) //C++ TChair for SsdOnGlobal
};

class StSsdSectorsOnGlobal : public St_SurveyC {
 public:
  static StSsdSectorsOnGlobal* 	instance();
  StSsdSectorsOnGlobal(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSsdSectorsOnGlobal() {fgInstance = 0;}
 private:
  static StSsdSectorsOnGlobal* fgInstance;
  ClassDef(StSsdSectorsOnGlobal,1) //C++ TChair for SsdSectorsOnGlobal
};

class StSsdLaddersOnSectors : public St_SurveyC {
 public:
  static StSsdLaddersOnSectors* 	instance();
  StSsdLaddersOnSectors(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSsdLaddersOnSectors() {fgInstance = 0;}
 private:
  static StSsdLaddersOnSectors* fgInstance;
  ClassDef(StSsdLaddersOnSectors,1) //C++ TChair for SsdLaddersOnSectors
};
class StSsdWafersOnLadders : public St_SurveyC {
 public:
  static StSsdWafersOnLadders* 	instance();
  StSsdWafersOnLadders(St_Survey *table=0) : St_SurveyC(table) {}
  virtual ~StSsdWafersOnLadders() {fgInstance = 0;}
 private:
  static StSsdWafersOnLadders* fgInstance;
  ClassDef(StSsdWafersOnLadders,1) //C++ TChair for SsdWafersOnLadders
};

#endif
