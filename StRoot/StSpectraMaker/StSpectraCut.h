#ifndef StSpectraCut_hh
#define StSpectraCut_hh

#ifdef SOLARIS 
 #ifndef false
  typedef int bool;
  #define false 0
  #define true 1
 #endif
#endif

#include "StEvent.h"

class StSpectraCut {

 private:

 protected:

 public:

  virtual  ~StSpectraCut() {};
 
  virtual bool satisfiesCut(StGlobalTrack* track, StEvent* event) = 0;

};

#endif
