#ifndef StPath2tpxGain_h
#define StPath2tpxGain_h

#include "St_FilePathC.h"

class StPath2tpxGain : public St_FilePathC {
 public:
  static StPath2tpxGain*         instance();
  StPath2tpxGain(St_FilePath *table=0) : St_FilePathC(table) {}
  virtual ~StPath2tpxGain() {fgInstance = 0;}
 private:
  static StPath2tpxGain* fgInstance;
  ClassDef(StPath2tpxGain,1) //C++ TChair for SsdOnGlobal
};

#endif
