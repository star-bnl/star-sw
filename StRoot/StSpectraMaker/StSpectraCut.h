#ifndef StSpectraCut_hh
#define StSpectraCut_hh

#include "StEvent.h"

class StSpectraCut {

 private:

 protected:

 public:

  virtual  ~StSpectraCut() {};
 
  virtual bool satisfiesCut(const StGlobalTrack* track, const StEvent* event) = 0;

};

#endif
