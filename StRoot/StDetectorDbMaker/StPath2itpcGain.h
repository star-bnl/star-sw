#ifndef StPath2itpcGain_h
#define StPath2itpcGain_h

#include "St_FilePathC.h"

class StPath2itpcGain : public St_FilePathC {
 public:
  static StPath2itpcGain*         instance();
  StPath2itpcGain(St_FilePath *table=0) : St_FilePathC(table) {}
  virtual ~StPath2itpcGain() {fgInstance = 0;}
 private:
  static StPath2itpcGain* fgInstance;
  ClassDef(StPath2itpcGain,1) //C++ TChair for SsdOnGlobal
};

#endif
